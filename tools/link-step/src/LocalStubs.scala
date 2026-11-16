package linkstep

import scala.collection.mutable
import scala.jdk.CollectionConverters._
import scala.reflect.internal.Flags
import scala.tools.asm.Opcodes
import scala.tools.asm.tree.{ClassNode, MethodNode}

/** Best-effort class symbols for class files that have no pickle: local and anonymous classes.
 *
 *  Parents come from the class's generic `Signature` attribute, or from its superclass and interfaces when there is
 *  none. A specialized parent such as `Order$mcI$sp` becomes the generic `Order[Int]`, so that the specialize phase
 *  picks the variant itself. Type variables the class does not declare (those of the enclosing method) become abstract
 *  type members. `Object` becomes `Any` and `BoxedUnit` becomes `Unit`, as Scala emits them for those. Java signatures
 *  have no kinds: where a type constructor is expected and the signature has a wildcard or a plain type variable, a
 *  fresh abstract type constructor of the right arity stands in.
 *
 *  The class's own members are guessed from the class file: every instance method that is not a bridge, a trait
 *  setter, a trait super accessor, an outer accessor of a trait, a specialized variant, or shaped like a forwarder to
 *  a trait's implementation (`T.m$`), and that is not the accessor of a trait field, a trait var or an object inherited
 *  from a trait. An own method whose descriptor is the erasure of a member it overrides takes that member's type as
 *  seen from the class; otherwise its type comes from its signature or descriptor. Method bodies are not needed.
 *
 *  The stub is a top-level class of the package. Placing it in its enclosing class would give it an outer pointer,
 *  but the phases expect a class's tree inside its owner's tree, and the enclosing class has no tree here. So a class
 *  whose parent is an inner class cannot be relinked this way.
 */
trait LocalStubs { self: Relinker =>
  import global._
  import JvmSig._

  /** Names (bytecode) of the members a local stub took as the class's own: methods, then fields. */
  val localOwn = mutable.LinkedHashMap[String, Own]()

  /** Type variables of one stub: the class's type parameters, abstract type members for the rest, and abstract type
   *  constructors where a kind is needed that the signature does not give. */
  private final class Ctx(cls: Symbol, decls: Scope) {
    val env = mutable.Map[String, Type]()
    /** Abstract type constructors that stand for a wildcard, with the type parameter they are an argument for. */
    val wildcards = mutable.LinkedHashMap[Symbol, Symbol]()
    private var fresh = 0
    def tvar(name: String): Type = env.getOrElseUpdate(name, {
      val t = cls.newAbstractType(TypeName(name), NoPosition, Flags.DEFERRED).setInfo(TypeBounds.empty)
      decls.enter(t)
      t.tpeHK
    })
    def hk(name: Option[String], param: Symbol): Type = {
      val key = name.getOrElse { fresh += 1; "_" + fresh } + "$hk" + param.typeParams.size
      env.getOrElseUpdate(key, {
        val t = cls.newAbstractType(TypeName(key), NoPosition, Flags.DEFERRED)
        val tps = cloneSymbolsAtOwner(param.typeParams, t)
        t.setInfo(genPolyType(tps, TypeBounds.empty))
        decls.enter(t)
        if (name.isEmpty) wildcards(t) = param
        t.typeConstructor
      })
    }
  }

  /** A class symbol for the class file `cn`, named by its binary name, or a reason why there is none. */
  def localStub(binaryName: String, cn: ClassNode): Either[String, Symbol] = {
    val slash = binaryName.lastIndexOf('/')
    val pkgName = if (slash < 0) "" else binaryName.substring(0, slash).replace('/', '.')
    // a name with `$anon` in it makes the backend look for the enclosing class of an anonymous class
    val simple = LocalStubs.stubName(binaryName.substring(slash + 1))
    val pkg = if (pkgName.isEmpty) rootMirror.EmptyPackageClass else rootMirror.getPackageIfDefined(TermName(pkgName)).moduleClass
    if (pkg == NoSymbol) return Left("no package")
    val isTrait = (cn.access & Opcodes.ACC_INTERFACE) != 0

    val cls = pkg.info.decl(TypeName(simple)) match {
      case c: ClassSymbol => c
      case _ =>
        val c = pkg.newClassSymbol(TypeName(simple), NoPosition, 0L)
        pkg.info.decls.enter(c)
        c
    }
    cls.resetFlag(Flags.JAVA | Flags.INTERFACE | Flags.TRAIT | Flags.ABSTRACT | Flags.FINAL | Flags.DEFERRED)
    cls.setFlag(if (isTrait) Flags.TRAIT | Flags.INTERFACE | Flags.ABSTRACT else 0L)
    if ((cn.access & Opcodes.ACC_FINAL) != 0) cls.setFlag(Flags.FINAL)
    if ((cn.access & Opcodes.ACC_ABSTRACT) != 0 && !isTrait) cls.setFlag(Flags.ABSTRACT)

    val decls = newScope
    val ctx = new Ctx(cls, decls)
    val sig = Option(cn.signature).map(classSig)

    val classTParams = sig.toList.flatMap(_.tparams).map(tp => cls.newTypeParameter(TypeName(tp.name)) -> tp)
    classTParams.foreach { case (s, _) => ctx.env(s.name.toString) = s.tpeHK }
    classTParams.foreach { case (s, tp) => s.setInfo(TypeBounds.upper(tp.bounds.headOption.map(toType(_, ctx)).filter(_ != NoType).getOrElse(definitions.AnyTpe))) }

    val parentSigs: List[Cls] = sig match {
      case Some(s) => s.superclass :: s.interfaces
      case None    => Cls(cn.superName, Nil) :: cn.interfaces.asScala.toList.map(Cls(_, Nil))
    }
    val parents0 = parentSigs.map(p => parentType(p, ctx)) match {
      case p :: ps if p.typeSymbol == definitions.ObjectClass || p.typeSymbol == definitions.AnyClass => definitions.AnyRefTpe :: ps
      case ps => ps
    }
    val parents = fillWildcards(parents0, ctx, cn)
    if (parents.exists(_ == NoType)) return Left("unresolved parent " + parentSigs.zip(parents0).collectFirst { case (s, NoType) => s.binaryName }.get)

    // the parents are known; the members are entered into `decls` below, as they are made
    val info = ClassInfoType(parents, decls, cls)
    cls.setInfo(if (classTParams.isEmpty) info else PolyType(classTParams.map(_._1), info))

    val traitMembers = cls.info.baseClasses.tail.filter(_.isTrait).flatMap(_.info.decls)
    // concrete accessors of trait vals and vars, and objects of traits: the class implements them itself
    val traitAccessors = traitMembers.filter(d => d.isAccessor && !d.isDeferred).map(_.name.toString).toSet
    val traitObjects = traitMembers.filter(_.isModule).map(_.name.toString).toSet
    val fieldNames = cn.fields.asScala.map(_.name).toSet
    val traitFieldNames = cn.methods.asScala.flatMap(m => setterField(m.name)).toSet
    val ownMethods = cn.methods.asScala.toList.filter(m =>
      isOwn(m, fieldNames, traitFieldNames) && !(traitAccessors(m.name) && isFieldAccessor(m)) && !(traitObjects(m.name) && m.desc.startsWith("()")))
    for (m <- ownMethods) decls.enter(ownMethod(cls, m, ctx))
    if (!isTrait) decls.enter(cls.newClassConstructor(NoPosition))
    else if (cn.methods.asScala.exists(_.name == "$init$"))
      decls.enter(cls.newMethod(nme.MIXIN_CONSTRUCTOR, NoPosition).setInfo(MethodType(Nil, definitions.UnitTpe)))
    val ownFields = cn.fields.asScala.map(_.name).filterNot(n => traitFieldNames(n) || n.endsWith("$module") || n.startsWith("bitmap$") || n == "$outer").toSet
    localOwn(LocalStubs.stubName(binaryName)) = Own(ownMethods.map(_.name).toSet, ownFields)
    Right(cls)
  }

  /** A type-constructor argument the signature wrote as `*` (`CommutativeMonad<*>` for `CommutativeMonad[Tuple2[A, *]]`)
   *  is often the same as another parent's: in `X[F] extends Y[F]`, the `F` that another parent passes to `Y`. Take it
   *  from there, so the two parents agree on their common base classes. */
  private def fillWildcards(parents: List[Type], ctx: Ctx, cn: ClassNode): List[Type] = {
    if (ctx.wildcards.isEmpty || parents.contains(NoType)) return parents
    val solved = mutable.LinkedHashMap[Symbol, Type]()
    for ((placeholder, param) <- ctx.wildcards) {
      val c = param.owner
      val candidates = for {
        b <- c.baseClasses.tail.iterator
        idx = c.tpe_*.baseType(b).typeArgs.indexWhere(_.typeSymbol == param)
        if idx >= 0
        q <- parents.iterator
        if !q.exists(_.typeSymbol == placeholder)
        bt = q.baseType(b)
        if bt != NoType && bt.typeArgs.size > idx
        arg = bt.typeArgs(idx)
        if !arg.exists(t => ctx.wildcards.contains(t.typeSymbol))
      } yield arg
      candidates.take(1).foreach(solved(placeholder) = _)
      if (!solved.contains(placeholder)) fromOwnMethods(placeholder, param, cn, ctx).foreach(solved(placeholder) = _)
    }
    if (solved.isEmpty) parents
    else {
      val subst = new TypeMap {
        def apply(t: Type): Type = t match {
          case TypeRef(_, sym, args) if solved.contains(sym) =>
            val to = solved(sym)
            if (args.isEmpty) to else appliedType(to, args.map(this))
          case _ => t.mapOver(this)
        }
      }
      parents.map(subst)
    }
  }

  /** Otherwise read it off the class's own methods. If the parent declares `def m[..A..](fa: F[A], ..)` (or returns
   *  `F[A]`) and the class
   *  has its own `m` whose generic signature takes `Const<TC;TA;>` at that position, with `TA` the method's type
   *  variable in `A`'s place, then `F` is `[X] => Const[C, X]`. Only unary `F` with one hole. */
  private def fromOwnMethods(placeholder: Symbol, param: Symbol, cn: ClassNode, ctx: Ctx): Option[Type] = {
    if (param.typeParams.size != 1) return None
    val c = param.owner
    val it = for {
      m <- cn.methods.asScala.iterator
      if m.signature != null && (m.access & (Opcodes.ACC_BRIDGE | Opcodes.ACC_STATIC)) == 0
      ms = try methodSig(m.signature) catch { case _: Throwable => null }
      if ms != null
      pm <- c.info.member(TermName(m.name)).alternatives.iterator
      if pm.isMethod
      pmTParams = pm.info.typeParams
      pmParams = pm.info.paramss.flatten
      if pmParams.size == ms.params.size && pmTParams.size == ms.tparams.size
      (ppt, own) <- (pmParams.map(_.tpe) :+ pm.info.finalResultType).iterator.zip((ms.params :+ ms.result).iterator)
      (f, hole) <- (ppt.dealias match { case TypeRef(_, f, List(h)) => Iterator((f, h)) case _ => Iterator.empty })
      if f == param && pmTParams.contains(hole.typeSymbol)
      ownHole = ms.tparams(pmTParams.indexOf(hole.typeSymbol)).name
      (name, args) <- (own match { case Cls(n, as) => Iterator((n, as)) case _ => Iterator.empty })
      holes = args.zipWithIndex.collect { case (Exact(TVar(`ownHole`)), i) => i }
      if holes.size == 1
      target = classSymbol(name)
      if target.isClass && target.typeParams.size == args.size
    } yield {
      val x = placeholder.newTypeParameter(TypeName("X")).setInfo(TypeBounds.empty)
      val fixed = args.zip(target.typeParams).zipWithIndex.map { case ((a, tp), i) => if (i == holes.head) x.tpeHK else argFor(a, tp, ctx) }
      typeFun(List(x), appliedType(target.typeConstructor, fixed))
    }
    it.take(1).toList.headOption
  }

  /** `T$_setter_$x_$eq` names trait field `x`. */
  private def setterField(name: String): Option[String] = {
    val i = name.indexOf("$_setter_$")
    if (i < 0 || !name.endsWith("_$eq")) None else Some(name.substring(i + "$_setter_$".length, name.length - "_$eq".length))
  }

  private def isFieldAccessor(m: MethodNode): Boolean = {
    val b = Compare.body(m)
    b.size <= 5 && b.exists(i => i.startsWith("GETFIELD") || i.startsWith("PUTFIELD") || i.startsWith("GETSTATIC") || i.startsWith("PUTSTATIC"))
  }

  private def isOwn(m: MethodNode, fields: Set[String], traitFields: Set[String]): Boolean = {
    val n = m.name
    val acc = m.access
    if (n == "<init>" || n == "<clinit>" || n == "$init$" || (acc & Opcodes.ACC_STATIC) != 0 || (acc & Opcodes.ACC_BRIDGE) != 0) false
    else if (n.contains("$_setter_$") || n.contains("$$super$") || n.endsWith("$$$outer") || n == "$outer") false
    else if (traitFields(n) && m.desc.startsWith("()")) false // accessor of a trait field
    else if (fields(n + "$module") && !fields.contains(n)) false // accessor of an object inherited from a trait
    else if (n.endsWith("$lzycompute") || n.contains("$lzycompute$")) false
    else if (SpecializedName.matches(n)) false // the specialize phase makes these from the generic method
    else {
      val b = Compare.body(m)
      val callsTraitStatic = b.exists(i => i.startsWith("INVOKESTATIC") && i.endsWith(" itf") && i.contains("$("))
      val callsTraitSpecial = b.exists(i => i.startsWith("INVOKESPECIAL") && i.endsWith(" itf"))
      val small = b.size <= Compare.argSlots(m.desc) + 4
      // forwarder to a trait's implementation, or a trait lazy val's accessor
      !((callsTraitStatic || callsTraitSpecial) && small) && !(b.exists(_.contains("$lzycompute")) && b.exists(_.contains("bitmap$")))
    }
  }

  private def ownMethod(cls: Symbol, m: MethodNode, ctx: Ctx): Symbol = {
    var flags = Flags.METHOD
    if ((m.access & Opcodes.ACC_PRIVATE) != 0) flags |= Flags.PRIVATE
    if ((m.access & Opcodes.ACC_PROTECTED) != 0) flags |= Flags.PROTECTED
    if ((m.access & Opcodes.ACC_FINAL) != 0) flags |= Flags.FINAL
    if ((m.access & Opcodes.ACC_ABSTRACT) != 0) flags |= Flags.DEFERRED
    val sym = cls.newMethod(TermName(m.name), NoPosition, flags)

    // the overridden member's type, when this method's descriptor is its erasure
    val overridden = cls.info.baseClasses.tail.iterator.flatMap(_.info.decl(TermName(m.name)).alternatives)
      .find(o => o.isMethod && !o.isPrivate && !o.isConstructor && descriptor(o, cls.thisType.memberType(o)) == m.desc)
    overridden match {
      case Some(o) => sym.setInfo(cls.thisType.memberType(o).cloneInfo(sym))
      case None =>
        val ms = methodSig(Option(m.signature).getOrElse(m.desc))
        val tparams = ms.tparams.map(tp => sym.newTypeParameter(TypeName(tp.name)) -> tp)
        val saved = tparams.map { case (s, tp) => tp.name -> ctx.env.put(tp.name, s.tpeHK) }
        def orAny(t: Type) = if (t == NoType) definitions.AnyTpe else t
        tparams.foreach { case (s, tp) => s.setInfo(TypeBounds.upper(orAny(tp.bounds.headOption.map(toType(_, ctx)).getOrElse(NoType)))) }
        val params = sym.newSyntheticValueParams(ms.params.map(p => orAny(toType(p, ctx))))
        val mt = MethodType(params, orAny(toType(ms.result, ctx)))
        saved.foreach { case (n, old) => old match { case Some(t) => ctx.env(n) = t case None => ctx.env.remove(n) } }
        sym.setInfo(if (tparams.isEmpty) mt else PolyType(tparams.map(_._1), mt))
    }
  }

  /** The JVM descriptor of the erasure of `tpe`, a type of method `m` (as seen from some class). */
  private def descriptor(m: Symbol, tpe: Type): String = {
    def d(t: Type): String = {
      val e = erasure.erasure(m)(t)
      e.typeSymbol match {
        case definitions.UnitClass    => "V"
        case definitions.BooleanClass => "Z"
        case definitions.ByteClass    => "B"
        case definitions.CharClass    => "C"
        case definitions.ShortClass   => "S"
        case definitions.IntClass     => "I"
        case definitions.LongClass    => "J"
        case definitions.FloatClass   => "F"
        case definitions.DoubleClass  => "D"
        case definitions.ArrayClass   => "[" + e.typeArgs.headOption.map(d).getOrElse("Ljava/lang/Object;")
        case s => "L" + exitingFlatten(s.javaBinaryNameString) + ";"
      }
    }
    try {
      "(" + tpe.paramss.flatten.map(p => d(p.tpe)).mkString + ")" + d(tpe.finalResultType)
    } catch { case _: Throwable => "" }
  }

  // ---- signatures to types ---------------------------------------------------------------------------------------

  private val SpecializedName = """(.*)\$mc([A-Z]+)\$sp""".r

  /** A parent type. A specialized variant `X$mcIJ$sp` becomes `X[...]` with the letters put back in the place of
   *  `X`'s specialized type parameters (sorted by name, as `SpecializeTypes.specializedName` does). */
  private def parentType(c: Cls, ctx: Ctx): Type = c.binaryName match {
    case SpecializedName(generic, letters) =>
      classSymbol(generic) match {
        case NoSymbol => NoType
        case g =>
          val tps = g.typeParams
          val spec = tps.filter(_.isSpecialized).sortBy(_.name.toString)
          if (spec.size != letters.length) NoType
          else {
            val bySpec = spec.zip(letters.map(primitive)).toMap
            val rest = c.args.iterator
            appliedType(g.typeConstructor, tps.map(tp => bySpec.getOrElse(tp, if (rest.hasNext) argFor(rest.next(), tp, ctx) else definitions.AnyTpe)))
          }
      }
    case _ => toType(c, ctx)
  }

  private def primitive(c: Char): Type = c match {
    case 'B' => definitions.ByteTpe
    case 'C' => definitions.CharTpe
    case 'D' => definitions.DoubleTpe
    case 'F' => definitions.FloatTpe
    case 'I' => definitions.IntTpe
    case 'J' => definitions.LongTpe
    case 'S' => definitions.ShortTpe
    case 'Z' => definitions.BooleanTpe
    case 'V' => definitions.UnitTpe
    case _   => definitions.AnyRefTpe
  }

  /** The type argument for type parameter `param`, minding its kind. */
  private def argFor(a: Arg, param: Symbol, ctx: Ctx): Type = {
    val arity = param.typeParams.size
    val sig = a match { case Wild => None case Exact(s) => Some(s) case Plus(s) => Some(s) case Minus(s) => Some(s) }
    if (arity == 0) sig.map(toType(_, ctx)).filter(_ != NoType).getOrElse(definitions.AnyTpe)
    else sig match {
      case Some(TVar(n)) => ctx.env.get(n).filter(_.typeParams.size == arity).getOrElse(ctx.hk(Some(n), param))
      case Some(Cls(name, Nil)) =>
        val s = classSymbol(name)
        if (s.typeParams.size == arity) s.typeConstructor else ctx.hk(None, param)
      case _ => ctx.hk(None, param)
    }
  }

  private def toType(s: Sig, ctx: Ctx): Type = s match {
    case Prim(c) => primitive(c)
    case TVar(n) => ctx.env.getOrElse(n, ctx.tvar(n)) match {
      case t if t.typeParams.nonEmpty => ctx.tvar(n + "$proper") // a constructor where a type is needed: give up on it
      case t => t
    }
    case Arr(e)  => definitions.arrayType(toType(e, ctx) match { case NoType => definitions.AnyTpe case t => t })
    case Cls(name, args) =>
      if (name == "java/lang/Object") definitions.AnyTpe
      else if (name == "scala/runtime/BoxedUnit") definitions.UnitTpe
      else classSymbol(name) match {
        case NoSymbol => NoType
        case sym =>
          val tps = sym.typeParams
          if (tps.isEmpty) sym.tpe_*
          else if (args.size == tps.size) appliedType(sym.typeConstructor, args.zip(tps).map { case (a, tp) => argFor(a, tp, ctx) })
          else appliedType(sym.typeConstructor, tps.map(tp => if (tp.typeParams.isEmpty) definitions.AnyTpe else ctx.hk(None, tp)))
      }
  }

  /** The class symbol for a binary name, following `$` into member classes and objects. The package also holds an
   *  entry for every nested class file (`Outer$Inner`), read as a Java class, so the member reached through the pickled
   *  outer class comes first. */
  def classSymbol(binaryName: String): Symbol = {
    val slash = binaryName.lastIndexOf('/')
    val pkgName = if (slash < 0) "" else binaryName.substring(0, slash).replace('/', '.')
    val pkg = if (pkgName.isEmpty) rootMirror.EmptyPackageClass else rootMirror.getPackageIfDefined(TermName(pkgName)) match {
      case NoSymbol => return NoSymbol
      case p => p.moduleClass
    }
    def module(owner: Symbol, name: String): Symbol = {
      val m = owner.info.decl(TermName(name))
      if (m.isModule && !m.isJavaDefined) m.moduleClass else NoSymbol
    }
    def member(owner: Symbol, rest: String): Symbol = {
      var i = rest.indexOf('$', 1)
      var found: Symbol = NoSymbol
      while (i > 0 && found == NoSymbol) {
        val head = rest.substring(0, i)
        val tail = rest.substring(i + 1)
        if (tail.nonEmpty) {
          val c = owner.info.decl(TypeName(head))
          if (c.isClass && !c.isJavaDefined) found = member(c, tail)
          val m = module(owner, head)
          if (found == NoSymbol && m != NoSymbol) found = member(m, tail)
        }
        i = rest.indexOf('$', i + 1)
      }
      if (found != NoSymbol) found
      else if (rest.endsWith("$") && module(owner, rest.stripSuffix("$")) != NoSymbol) module(owner, rest.stripSuffix("$"))
      else {
        val direct = owner.info.decl(TypeName(rest))
        if (direct.isClass) direct else NoSymbol
      }
    }
    try enteringPhase(currentRun.typerPhase)(member(pkg, binaryName.substring(slash + 1)))
    catch { case _: Throwable => NoSymbol }
  }
}

object LocalStubs {
  /** Set for `--local-all`: stubs of classes that have a pickle get a name of their own, so that the pickled symbol,
   *  which other classes see as their ancestor, stays as it is. */
  @volatile var separateNames = false
  private final val Marker = "$LinkStub"
  def stubName(binaryName: String): String = binaryName.replace("$anon", "$Anon") + (if (separateNames) Marker else "")
  def realName(stubName: String): String = stubName.replace(Marker, "").replace("$Anon", "$anon")
}
