package scala.tools.nsc

import java.net.URI
import java.nio.file.{FileSystems, Files, Path, Paths}

import org.junit.Assert.assertEquals
import org.junit.Test

import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters._
import scala.tools.asm.{ClassReader, ClassVisitor, MethodVisitor, Opcodes}
import scala.util.Using

/** The compiler and `scala.reflect.internal` must not materialize `TypeTag`s (`typeOf[T]`, `typeTag[T]`) for their
 *  own use. `TypeTag.apply` tests whether its mirror is a runtime `JavaMirror`, which loads that class into the
 *  compiler's class loader. Nothing links it in a plain compile, but a JDK AOT cache links every loaded class when it
 *  is written, and verifying `JavaMirror` loads `SynchronizedSymbol`. Its overrides of `Symbol.getFlag` and `validTo`
 *  then stop the JIT inlining those calls by class hierarchy analysis.
 *
 *  Look types up with `rootMirror.requiredClass[T]` or `definitions` instead.
 */
class NoTypeTagsInCompilerTest {
  private val TagFactories = Set("scala/reflect/api/TypeTags$TypeTag$", "scala/reflect/api/TypeTags$WeakTypeTag$")

  /** Classes that make tags for others, not for the compiler's own use. */
  private val Allowed = Set(
    "scala/reflect/macros/contexts/Aliases", // `c.TypeTag(mirror, creator)` and `c.WeakTypeTag(...)` for macro authors
    "scala/tools/reflect/StdTags",           // tags for the REPL and runtime universes
  )

  /** Where each scanned part was loaded from, and the package prefix scanned in it. */
  private def parts: List[(Class[_], String)] = List(
    classOf[Global]                              -> "scala/",
    classOf[scala.reflect.internal.SymbolTable] -> "scala/reflect/internal/",
  )

  private def withClassFiles[A](cls: Class[_])(f: Path => A): A = {
    val location = Paths.get(cls.getProtectionDomain.getCodeSource.getLocation.toURI)
    if (Files.isDirectory(location)) f(location)
    else Using.resource(FileSystems.newFileSystem(URI.create("jar:" + location.toUri), Map.empty[String, String].asJava))(fs => f(fs.getPath("/")))
  }

  private def tagMaterializations(root: Path, prefix: String): List[String] = {
    val found = ListBuffer.empty[String]
    Using.resource(Files.walk(root)) { paths =>
      paths.iterator.asScala.filter(p => p.toString.endsWith(".class") && root.relativize(p).toString.replace('\\', '/').startsWith(prefix)).foreach { p =>
        new ClassReader(Files.readAllBytes(p)).accept(new ClassVisitor(Opcodes.ASM9) {
          private var className = ""
          override def visit(version: Int, access: Int, name: String, signature: String, superName: String, interfaces: Array[String]): Unit =
            className = name
          override def visitMethod(access: Int, methodName: String, descriptor: String, signature: String, exceptions: Array[String]): MethodVisitor =
            new MethodVisitor(Opcodes.ASM9) {
              override def visitMethodInsn(opcode: Int, owner: String, name: String, descriptor: String, isInterface: Boolean): Unit =
                if (name == "apply" && TagFactories(owner) && !Allowed(className)) found += s"$className.$methodName calls $owner.apply"
            }
        }, ClassReader.SKIP_DEBUG | ClassReader.SKIP_FRAMES)
      }
    }
    found.toList.sorted
  }

  @Test def noTypeTagMaterialization(): Unit = {
    val found = parts.flatMap { case (cls, prefix) => withClassFiles(cls)(tagMaterializations(_, prefix)) }
    assertEquals("TypeTags materialized in the compiler (see NoTypeTagsInCompilerTest's doc)", "", found.mkString("\n"))
  }
}
