package linkstep

import scala.collection.mutable
import scala.reflect.internal.util.BatchSourceFile
import scala.reflect.io.{AbstractFile, VirtualDirectory, VirtualFile}
import scala.tools.nsc.{Global, Settings}
import scala.tools.nsc.reporters.StoreReporter

/** Regenerates the ancestor-derived parts of Scala class files from pickles alone.
 *
 *  For each requested top-level class or object, the class symbol is unpickled from the class path and a stub tree
 *  is synthesized for it: the class, its primary constructor (a bare `super.<init>(zeros)` call) and, recursively, its
 *  member classes and objects. None of the class's own method bodies or fields are in the tree. The stub is then run
 *  through the real post-pickler phases, uncurry to jvm, into memory. Whatever the backend emits beyond the stub is
 *  produced from symbols, that is from the class's pickle and its ancestors' pickles: mixin forwarders, trait fields,
 *  accessors and setters, bridges, trait super accessors, inherited module accessors, the `$init$` calls the erasure
 *  adds to the constructor, and static forwarders.
 *
 *  One `Relinker` is one `Global` and one `Run`. The phases mutate symbols (expanded names, added decls), so a
 *  `Relinker` is used once.
 */
final class Relinker(classpath: String, options: List[String], forceAncestorSpecialization: Boolean = true) {
  val settings = new Settings(msg => throw new IllegalArgumentException(msg))
  settings.processArguments(options, processAll = true)
  settings.classpath.value = classpath
  settings.nowarn.value = true
  val out = new VirtualDirectory("(relinked)", None)
  settings.outputDirs.setSingleOutput(out)
  val reporter = new StoreReporter(settings)
  val global: Global = new Global(settings, reporter)
  import global._

  /** Why a unit was dropped, by top-level binary name. */
  val failures = mutable.LinkedHashMap[String, String]()

  /** Top-level symbols found for a binary name in `pkg`, or Nil if it names no pickled top-level class or object. */
  def topLevel(binaryName: String): List[Symbol] = enteringPhase(currentRun.typerPhase) {
    val (pkgName, simple) = binaryName.lastIndexOf('.') match {
      case -1 => ("", binaryName)
      case i  => (binaryName.substring(0, i), binaryName.substring(i + 1))
    }
    val pkg = if (pkgName.isEmpty) rootMirror.EmptyPackageClass else rootMirror.getPackageIfDefined(TermName(pkgName)) match {
      case NoSymbol => NoSymbol
      case p => p.moduleClass
    }
    if (pkg == NoSymbol) Nil
    else {
      val decoded = simple // symbol names are kept encoded
      val cls = pkg.info.decl(TypeName(decoded))
      val mod = pkg.info.decl(TermName(decoded))
      List(cls, mod).filter(s => s.exists && s.owner == pkg && (s.isClass || s.isModule) && !s.isJavaDefined && !s.isPackage && isScalaPickled(s))
    }
  }

  private def isScalaPickled(s: Symbol): Boolean = {
    // forces the unpickler; a Java class or a class file without a pickle comes back Java-defined
    s.info
    !s.isJavaDefined
  }

  private def isRegenerable(s: Symbol): Boolean = s.exists && !s.isJavaDefined

  val run: Run = new Run
  // No frontend runs, so pretend it has: package objects are opened as their packages load, not deferred to the
  // packageobjects phase. Open the ones loaded so far.
  globalPhase = run.phaseNamed("uncurry")
  for (p <- analyzer.packageObjects.deferredOpen.toVector)
    if (analyzer.packageObjects.deferredOpen.remove(p)) openPackageModule(p, force = true)
  private[this] val units = mutable.LinkedHashMap[String, CompilationUnit]()

  /** Phase-time snapshot of each regenerated class's own pickled decls, by class symbol. */
  val ownDecls = mutable.LinkedHashMap[Symbol, List[Symbol]]()

  def relink(topLevelNames: Seq[String]): Unit = {

    for (name <- topLevelNames) {
      try {
        val syms = topLevel(name)
        if (syms.nonEmpty) {
          val file = new VirtualFile(name.replace('.', '/') + ".scala")
          val unit = new CompilationUnit(new BatchSourceFile(file, Array.emptyCharArray))
          unit.body = enteringPhase(currentRun.refchecksPhase.next)(packageDef(syms))
          syms.foreach(s => run.symSource(s) = file)
          units(name) = unit
        }
      } catch {
        case t: Throwable => failures(name) = "stub: " + describe(t)
      }
    }
    val addUnit = classOf[Global#Run].getDeclaredMethod("addUnit", classOf[CompilationUnit])
    addUnit.setAccessible(true)
    units.valuesIterator.foreach(u => addUnit.invoke(run, u))
    drivePhases()
  }

  private def describe(t: Throwable): String = {
    if (sys.props.contains("linkstep.debug")) t.printStackTrace()
    val root = Iterator.iterate(t)(_.getCause).takeWhile(_ != null).toList.last
    val msg = String.valueOf(root.getMessage).linesIterator.take(1).mkString
    s"${root.getClass.getSimpleName}: ${msg.take(300)}"
  }

  /** Runs the phases from uncurry to jvm. A unit that throws or reports an error in a tree phase is dropped (its body
   *  becomes empty) and recorded in `failures`, so one bad class does not take the batch with it. */
  private def drivePhases(): Unit = {
    var ph: scala.reflect.internal.Phase = run.phaseNamed("uncurry")
    val nameOf = units.map(_.swap)
    while (ph.hasNext) {
      globalPhase = ph
      phase = ph
      ph match {
        case _ if ph.name == "jvm" =>
          ph.run()
        case gp: GlobalPhase =>
          if (ph.name == "specialize" && forceAncestorSpecialization) forceSpecializedAncestors()
          for (u <- units.valuesIterator if u.body ne EmptyTree) {
            reporter.reset()
            try {
              gp.applyPhase(u)
              if (reporter.hasErrors) {
                failures(nameOf(u)) = s"${ph.name}: " + reporter.infos.filter(_.severity == reporter.ERROR).map(_.msg).mkString("; ").take(300)
                u.body = EmptyTree
              }
            } catch {
              case t: Throwable =>
                failures(nameOf(u)) = s"${ph.name}: " + describe(t)
                u.body = EmptyTree
            }
          }
          reporter.reset()
        case _ => ph.run()
      }
      ph = ph.next
    }
    globalPhase = ph
  }

  /** Specialization transforms the info of a class-path class only if something asks for it no later than the
   *  specialize phase (`SpecializeTypes.transformInfo0`). In a normal run the class's own trees ask for most of its
   *  ancestors; the stub does not. Ask for all of them, so that the specialized members of ancestors (and the
   *  bridges erasure derives from them) do not depend on what the class's own code happened to reference. */
  private def forceSpecializedAncestors(): Unit =
    for (cls <- ownDecls.keysIterator) {
      val c = if (cls.isModule) cls.moduleClass else cls
      try exitingPhase(currentRun.specializePhase)(c.baseClasses.foreach(_.info))
      catch { case _: Throwable => }
    }

  // ---- stub trees ------------------------------------------------------------------------------------------------

  private def packageDef(syms: List[Symbol]): Tree = {
    val pkg = syms.head.owner
    val pid = if (pkg.isEmptyPackageClass) Ident(nme.EMPTY_PACKAGE_NAME).setSymbol(pkg.sourceModule).setType(NoType)
              else gen.mkAttributedRef(pkg.sourceModule)
    val tree = PackageDef(pid.asInstanceOf[RefTree], syms.map(definition)).setSymbol(pkg.sourceModule)
    tree.foreach(t => if (t.tpe eq null) t.setType(NoType)) // definitions carry NoType after the typer
    tree
  }

  private def definition(sym: Symbol): Tree =
    if (sym.isModule) ModuleDef(sym, template(sym.moduleClass))
    else ClassDef(sym, template(sym))

  private def template(cls: Symbol): Template = {
    val decls = cls.info.decls.toList
    ownDecls(cls) = decls
    val nested = decls.filter(d => isRegenerable(d) && (d.isModule || (d.isClass && !d.isModuleClass && !d.isAliasType && !d.isAbstractType)))
    val body = constructor(cls).toList ::: nested.map(definition)
    val dummy = cls.newLocalDummy(cls.pos)
    Template(cls.info.parents.map(TypeTree(_)), noSelfType, body).setSymbol(dummy).setType(NoType)
  }

  private def unitLit = Literal(Constant(())).setType(definitions.UnitTpe)

  private def constructor(cls: Symbol): Option[DefDef] = {
    val ctor = cls.primaryConstructor
    if (!ctor.exists) None
    else if (cls.isTrait) Some(DefDef(ctor, Block(Nil, unitLit).setType(definitions.UnitTpe)))
    else Some(DefDef(ctor, Block(List(superCall(cls)), unitLit).setType(definitions.UnitTpe)))
  }

  /** `super.<init>(zero, ...)` against the superclass's primary constructor. Repeated parameters get no arguments. */
  private def superCall(cls: Symbol): Tree = {
    val superTpe = cls.info.parents.head
    val superCtor = superTpe.typeSymbol.primaryConstructor
    val thisTree = gen.mkAttributedThis(cls)
    val sup = Super(thisTree, tpnme.EMPTY).setType(SuperType(cls.thisType, superTpe)).setSymbol(cls)
    var fn: Tree = Select(sup, superCtor).setType(superTpe.memberType(superCtor))
    var tpe = fn.tpe
    while (tpe.isInstanceOf[MethodType]) {
      val mt = tpe.asInstanceOf[MethodType]
      val args = mt.params.filterNot(p => definitions.isRepeatedParamType(p.tpe)).map(p => gen.mkZero(definitions.dropByName(p.tpe)))
      fn = Apply(fn, args).setType(mt.resultType)
      tpe = mt.resultType
    }
    fn
  }

  // ---- results ---------------------------------------------------------------------------------------------------

  /** The class files the backend wrote, by binary name with '/' separators. */
  def outputs: Map[String, Array[Byte]] = {
    val b = Map.newBuilder[String, Array[Byte]]
    def walk(dir: AbstractFile, prefix: String): Unit = dir.iterator.foreach { f =>
      if (f.isDirectory) walk(f, prefix + f.name + "/")
      else if (f.name.endsWith(".class")) b += ((prefix + f.name.stripSuffix(".class"), f.toByteArray))
    }
    walk(out, "")
    b.result()
  }

  /** The bytecode names of each regenerated class's own pickled members, read after the run (names may have been
   *  expanded by then). Keyed by the class's binary name. */
  def ownMembers: Map[String, Own] = exitingPhase(currentRun.jvmPhase) {
    ownDecls.iterator.map { case (cls, decls) =>
      def name(d: Symbol) = d.javaSimpleName.toString.trim
      val names = decls.flatMap(d => if (d.isLazy) List(name(d), name(d) + "$lzycompute") else List(name(d)))
      val fields = decls.filter(d => d.isTerm && (!d.isMethod || d.isLazy) && !d.isModule).map(name) ++
        decls.filter(_.isModule).map(name(_) + "$module")
      binaryName(cls) -> Own(names.toSet, fields.toSet)
    }.toMap
  }

  def binaryName(cls: Symbol): String = exitingPhase(currentRun.jvmPhase) {
    val c = if (cls.isModule) cls.moduleClass else cls
    c.javaBinaryNameString.replace('.', '/')
  }
}
