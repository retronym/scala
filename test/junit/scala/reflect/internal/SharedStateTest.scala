package scala.reflect.internal

import java.io.File
import java.lang.reflect.{Field, Modifier}
import java.nio.file.{Files, Paths}
import java.util.zip.ZipFile

import org.junit.Assert.fail
import org.junit.Test

import scala.collection.mutable
import scala.jdk.CollectionConverters._
import scala.tools.asm.{ClassReader, ClassVisitor, MethodVisitor, Opcodes}
import scala.util.Using

/** Finds mutable state that a universe shares between threads, in the class files of scala-reflect
 *  and scala-compiler, so that a new piece fails here rather than racing at runtime. The scratch
 *  state of an operation belongs in `SymbolTable#Scratch`; anything else shared must be listed with
 *  what protects it. A listed field that is no longer found fails too.
 */
class SharedStateTest {
  import SharedStateTest._

  /** Type maps, collectors, traversers and transformers keep the state of one traversal in fields.
   *  An `object` of one, or a field holding one, whose enclosing instance is shared between threads
   *  (a universe or one of its components, another object, a type or a symbol) is shared by every
   *  thread that uses the universe.
   */
  @Test def sharedTraversals(): Unit = {
    val classes = (classNames(classOf[SymbolTable]) ++ classNames(classOf[scala.tools.nsc.Global])).distinct.flatMap(load)
    // A class is a singleton per universe if a component or an object is an instance of it.
    val singletons = components(classes) ++ classes.filter(isModuleClass)
    def isShared(c: Class[_]) = isSharedKind(c) || singletons.exists(s => c.isAssignableFrom(s) || s.isAssignableFrom(c))
    val objects = for {
      c <- classes if isTraversal(c) && isModuleClass(c) && enclosing(c).forall(isShared) && traversalState(c).nonEmpty
    } yield s"${c.getName}" -> traversalState(c).map(_.getName).mkString(", ")
    val held = for {
      u <- classes if isShared(u)
      f <- instanceFields(u) if isTraversal(f.getType) && traversalState(f.getType).nonEmpty
    } yield s"${u.getName}.${f.getName}" -> f.getType.getName
    check("shared traversals", (objects ++ held).toMap, allowedTraversals)
  }

  /** The vars and mutable collections of a runtime reflection universe, its objects and components.
   *  Symbols, types, scopes, trees and traversals have disciplines of their own and are left out.
   */
  @Test def sharedStateOfRuntimeReflection(): Unit = {
    val found = for {
      c <- sharedClasses(classOf[scala.reflect.runtime.JavaUniverse])
      (f, kind) <- mutableState(c)
    } yield s"${c.getName}.${plain(f.getName)}" -> s"$kind ${f.getType.getName}"
    check("shared fields of runtime reflection", found.toMap, allowedState)
  }

  private def check(what: String, found: Map[String, String], allowed: Map[String, String]): Unit = {
    val problems = found.keys.filterNot(allowed.contains).toList.sorted.map(k => s"$k: ${found(k)}")
    val stale = allowed.keys.filterNot(found.contains).toList.sorted
    if (problems.nonEmpty) fail(problems.mkString(s"${problems.size} unlisted $what:\n  ", "\n  ", ""))
    if (stale.nonEmpty) fail(stale.mkString(s"listed $what, but no longer found:\n  ", "\n  ", ""))
  }
}

object SharedStateTest {
  private final val CompilerOnly = "only a compiler universe uses it, and one runs on one thread at a time (a toolbox holds `toolBoxLock`)"
  private final val Init         = "written only while the universe is built"
  private final val OnePhase     = "runtime reflection has one phase, so a write stores the value already there"
  private final val Gil          = "runtime reflection writes it under the GIL"
  private final val NameLock     = "under `nameLock` in a concurrent universe (`synchronizeNames`)"
  private final val Debug        = "only with a debugging or tracing option"
  private final val Counter      = "a statistics counter: a racing increment loses a count, nothing else"
  private final val Volatile     = "volatile, and a racing write stores an equivalent value"

  private val allowedTraversals: Map[String, String] = Map(
    "scala.reflect.internal.Types$substTypeMapCache$.cached" -> CompilerOnly,
    "scala.reflect.internal.transform.Erasure$boxingErasure$" -> "a concurrent universe makes its own, see `Erasure.erasedValueClassArg`",
    "scala.reflect.internal.transform.Erasure$boxing3Erasure$" -> "a concurrent universe makes its own, see `Erasure.erasedValueClassArg`",
    "scala.reflect.internal.Variances$VarianceValidator$ValidateVarianceMap$" -> s"shared through `RefChecks`' `varianceValidator`: $CompilerOnly",
    "scala.tools.nsc.typechecker.RefChecks.toJavaRepeatedParam" -> CompilerOnly,
    "scala.tools.nsc.typechecker.RefChecks.toScalaRepeatedParam" -> CompilerOnly,
    "scala.tools.nsc.transform.async.TransformUtils$containsAwaitTraverser$" -> CompilerOnly,
    "scala.tools.nsc.typechecker.Infer$instantiate$" -> CompilerOnly,
    "scala.tools.nsc.typechecker.TreeCheckers$SymbolTracker$" -> CompilerOnly,
  )

  private val allowedState: Map[String, String] = Map(
    "scala.reflect.internal.Definitions$DefinitionsClass.isInitialized" -> Init,
    "scala.reflect.internal.Definitions$DefinitionsClass.samCache" -> s"`samOf` is only reached from the typer's prototypes: $CompilerOnly",
    "scala.reflect.internal.SymbolTable$perRunCaches$.caches" -> s"$Init; a compiler universe records more: $CompilerOnly",
    "scala.reflect.internal.SymbolTable$perRunCaches$.javaCaches" -> s"$Init; a compiler universe records more: $CompilerOnly",
    "scala.reflect.internal.SymbolTable$traceSymbols$.allChildren" -> Debug,
    "scala.reflect.internal.SymbolTable$traceSymbols$.allSymbols" -> Debug,
    "scala.reflect.internal.SymbolTable$traceSymbols$.allTrees" -> Debug,
    "scala.reflect.internal.SymbolTable$traceSymbols$.enabled" -> Debug,
    "scala.reflect.internal.SymbolTable$traceSymbols$.prevOwners" -> Debug,
    "scala.reflect.internal.SymbolTable._chrs" -> NameLock,
    "scala.reflect.internal.SymbolTable._intersectionWitness" -> s"`CompoundType.memo`, from `defineBaseTypeSeqOfCompoundType` and `defineBaseClassesOfCompoundType`: $Gil",
    "scala.reflect.internal.SymbolTable.explainSwitch" -> Debug,
    "scala.reflect.internal.SymbolTable.infoTransformers" -> s"$Init; a compiler universe installs its phases' transformers: $CompilerOnly",
    "scala.reflect.internal.SymbolTable.keepPhaseStack" -> Debug,
    "scala.reflect.internal.SymbolTable.nc" -> NameLock,
    "scala.reflect.internal.SymbolTable.nextFrom" -> s"$Init; a compiler universe installs its phases' transformers: $CompilerOnly",
    "scala.reflect.internal.SymbolTable.nodeCount" -> Counter,
    "scala.reflect.internal.SymbolTable.originalOwnerMap" -> s"recorded by the phases after the typer: $CompilerOnly",
    "scala.reflect.internal.SymbolTable.per" -> OnePhase,
    "scala.reflect.internal.SymbolTable.ph" -> OnePhase,
    "scala.reflect.internal.SymbolTable.phStack" -> Debug,
    "scala.reflect.internal.SymbolTable.phaseWithId" -> Init,
    "scala.reflect.internal.SymbolTable.scopeCount" -> Counter,
    "scala.reflect.internal.SymbolTable.termHashtable" -> NameLock,
    "scala.reflect.internal.SymbolTable.typeHashtable" -> NameLock,
    "scala.reflect.internal.Symbols$CyclicReference$.emptyTrace" -> "an empty array",
    "scala.reflect.internal.Types$substTypeMapCache$.cached" -> CompilerOnly,
    "scala.reflect.internal.transform.Transforms$Lazy.value" -> Volatile,
  )

  private val loader = getClass.getClassLoader
  private val mirror = scala.reflect.runtime.universe.runtimeMirror(loader)

  private def load(name: String): Option[Class[_]] =
    try Some(Class.forName(name, false, loader)) catch { case _: Throwable => None }
  private def loadAll(names: String*): List[Class[_]] = names.toList.flatMap(load)

  private val traversals = loadAll(
    "scala.reflect.api.Trees$Traverser", "scala.reflect.api.Trees$Transformer",
    "scala.reflect.internal.Trees$InternalTraverser", "scala.reflect.internal.Trees$InternalTransformer",
    "scala.reflect.internal.tpe.TypeMaps$TypeMap", "scala.reflect.internal.tpe.FindMembers$FindMemberBase")
  private val sharedData = List(classOf[SymbolTable], classOf[Types#Type], classOf[Symbols#Symbol])
  private val dataClasses = loadAll(
    "scala.reflect.internal.Symbols$Symbol", "scala.reflect.internal.Types$Type", "scala.reflect.internal.Scopes$Scope",
    "scala.reflect.internal.Trees$Tree", "scala.reflect.internal.AnnotationInfos$AnnotationInfo") ++ traversals
  private val mutableTypes = loadAll(
    "scala.collection.mutable.Iterable", "scala.collection.mutable.Clearable", "java.util.Collection", "java.util.Map",
    "java.lang.StringBuilder", "scala.reflect.internal.util.WeakHashSet", "scala.reflect.internal.util.ReusableInstance",
    "java.lang.ThreadLocal", "java.util.concurrent.atomic.AtomicInteger", "java.util.concurrent.atomic.AtomicLong")

  private def isTraversal(c: Class[_]) = traversals.exists(_.isAssignableFrom(c))
  private def isData(c: Class[_]) = dataClasses.exists(_.isAssignableFrom(c))
  private def isMutableType(t: Class[_]) = t.isArray || mutableTypes.exists(_.isAssignableFrom(t))

  private def isModuleClass(c: Class[_]): Boolean =
    c.getName.endsWith("$") && (try mirror.classSymbol(c).isModuleClass catch { case _: Throwable => false })

  private def hierarchy(c: Class[_]): List[Class[_]] =
    Iterator.iterate[Class[_]](c)(_.getSuperclass).takeWhile(_ != null).toList

  private def instanceFields(c: Class[_]): List[Field] =
    c.getDeclaredFields.toList.filterNot(f => Modifier.isStatic(f.getModifiers) || f.getName == "$outer" || f.getName.endsWith("$module"))

  /** The name of a field without the prefix of an expanded name. */
  private def plain(name: String) = name.substring(name.lastIndexOf("$$") match { case -1 => 0 case i => i + 2 })

  private def classNames(of: Class[_]): List[String] = {
    val root = Paths.get(of.getProtectionDomain.getCodeSource.getLocation.toURI)
    def toName(rel: String) = rel.stripSuffix(".class").replace('/', '.').replace(File.separatorChar, '.')
    if (Files.isDirectory(root))
      Using.resource(Files.walk(root))(_.iterator.asScala.filter(_.toString.endsWith(".class")).map(p => toName(root.relativize(p).toString)).toList)
    else
      Using.resource(new ZipFile(root.toFile))(_.entries.asScala.map(_.getName).filter(_.endsWith(".class")).map(toName).toList)
  }

  // ---- traversals

  /** The mutable fields of a traversal. `currentOwner` only counts if it is read: `atOwner` writes it,
   *  and reads it only to restore it, so racing writes alone are harmless.
   */
  private def traversalState(c: Class[_]): List[Field] =
    hierarchy(c).flatMap(_.getDeclaredFields)
      .filter(f => !Modifier.isStatic(f.getModifiers) && !Modifier.isFinal(f.getModifiers))
      .filter(f => f.getName != "currentOwner" || readsCurrentOwner(c))

  private val ownerBases = Set("scala.reflect.api.Trees$Traverser", "scala.reflect.api.Trees$Transformer")
  private val ownerReaders = Set("currentOwner", "currentMethod", "currentClass")

  /** Does code in `c` or its superclasses, other than the `api` bases that define `atOwner`, read `currentOwner`? */
  private def readsCurrentOwner(c: Class[_]): Boolean =
    hierarchy(c).filterNot(k => ownerBases(k.getName)).exists { k =>
      val in = loader.getResourceAsStream(k.getName.replace('.', '/') + ".class")
      in != null && Using.resource(in) { in =>
        var reads = false
        new ClassReader(in).accept(new ClassVisitor(Opcodes.ASM9) {
          override def visitMethod(access: Int, name: String, desc: String, sig: String, exns: Array[String]): MethodVisitor =
            new MethodVisitor(Opcodes.ASM9) {
              override def visitMethodInsn(op: Int, owner: String, name: String, desc: String, itf: Boolean): Unit =
                if (ownerReaders(name) && desc.startsWith("()")) reads = true
              override def visitFieldInsn(op: Int, owner: String, name: String, desc: String): Unit =
                if (op == Opcodes.GETFIELD && name == "currentOwner") reads = true
            }
        }, ClassReader.SKIP_DEBUG | ClassReader.SKIP_FRAMES)
        reads
      }
    }

  /** Is an instance of `c` shared between threads: a universe, a cake slice, an object, a type or a symbol? */
  private def isSharedKind(c: Class[_]): Boolean =
    c.isInterface || isModuleClass(c) || sharedData.exists(_.isAssignableFrom(c))

  /** The classes of the components a universe holds in its fields, such as `analyzer`. */
  private def components(classes: List[Class[_]]): Set[Class[_]] =
    classes.filter(classOf[SymbolTable].isAssignableFrom).flatMap(_.getDeclaredFields).map(_.getType)
      .filter(t => !t.isPrimitive && !t.isInterface && t.getName.startsWith("scala.") && !t.getName.startsWith("scala.collection.")).toSet

  private def enclosing(c: Class[_]): Option[Class[_]] = {
    val name = c.getName.stripSuffix("$")
    val i = name.lastIndexOf('$')
    if (i <= 0) None else load(name.substring(0, i)).orElse(load(name.substring(0, i) + "$"))
  }

  // ---- fields of a universe

  /** The vars (not lazy vals, nor the vals of traits) and the mutable collections in the fields of `c`. */
  private def mutableState(c: Class[_]): List[(Field, String)] = {
    val methods = hierarchy(c).flatMap(_.getDeclaredMethods).map(_.getName).toSet
    def isLazy(f: Field) = methods.exists(_.endsWith(plain(f.getName) + "$lzycompute"))
    def isTraitVal(f: Field) = methods.exists(m => m.contains("$_setter_$") && m.endsWith("$" + plain(f.getName) + "_$eq"))
    instanceFields(c).filterNot(f => f.getName.startsWith("bitmap$") || isLazy(f)).flatMap { f =>
      if (!Modifier.isFinal(f.getModifiers) && !isTraitVal(f)) List(f -> (if (Modifier.isVolatile(f.getModifiers)) "volatile var" else "var"))
      else if (isMutableType(f.getType)) List(f -> "mutable")
      else Nil
    }
  }

  /** The classes of the instances a universe holds: itself, its objects, and the components they hold. */
  private def sharedClasses(root: Class[_]): List[Class[_]] = {
    val seen = mutable.LinkedHashSet[Class[_]]()
    def isComponent(t: Class[_]) = !t.isPrimitive && !t.isArray && t.getName.startsWith("scala.reflect.internal.") &&
      (t.getName.contains("internal.tpe.") || t.getName.contains("internal.transform.") || t.getName.endsWith("Definitions$DefinitionsClass"))
    def visit(c: Class[_]): Unit = if (c.getName.startsWith("scala.") && !c.isInterface && !isData(c) && seen.add(c)) {
      for (f <- c.getDeclaredFields if !Modifier.isStatic(f.getModifiers) && f.getName != "$outer")
        if (f.getName.endsWith("$module") || isComponent(f.getType)) visit(f.getType)
      Option(c.getSuperclass).foreach(visit)
    }
    visit(root)
    seen.toList
  }
}
