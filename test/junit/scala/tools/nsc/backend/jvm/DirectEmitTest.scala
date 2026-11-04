package scala.tools.nsc.backend.jvm

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.tools.asm.tree.ClassNode
import scala.tools.nsc.reporters.StoreReporter
import scala.tools.testkit.BytecodeTesting._
import scala.tools.testkit.Compiler

/** Classes written directly from codegen against the same classes built as a `ClassNode`. */
@RunWith(classOf[JUnit4])
class DirectEmitTest {
  import DirectEmitTest._

  private def text(nodes: List[ClassNode]) = nodes.map(AsmUtils.textify(_))

  private def assertSameClasses(code: String): Unit =
    assertEquals(text(tree.compileClasses(code, allowMessage = _ => true)), text(direct.compileClasses(code, allowMessage = _ => true)))

  @Test def innerClassesAndSignatures(): Unit = assertSameClasses(
    """import scala.annotation.ClassfileAnnotation
      |class Ann(c: Class[_]) extends scala.annotation.StaticAnnotation with ClassfileAnnotation
      |class O { class I; object M { class J } }
      |object T { class N[A]; trait U }
      |@Ann(classOf[T.U]) class C[A <: T.U](@deprecated val o: O) {
      |  @Ann(classOf[T.N[_]]) val i: T.N[A] = null
      |  @throws[java.io.IOException] def f(n: T.N[String]): Array[o.I] = { new o.I; null }
      |  def g = { class L; new L; classOf[T.N[_]]; Array.ofDim[o.M.J](1, 2) }
      |  def h(x: Any) = try x.asInstanceOf[T.U] catch { case _: Map.WithDefault[_, _] => null }
      |}
      |""".stripMargin)

  @Test def modulesAndMirrors(): Unit = assertSameClasses(
    """object A { val x = 1; def f(y: Int): Int = y + x }
      |object B { val x = if (A.x > 0) "a" else return_; def return_ = "b" }
      |class D
      |object D { def g = 2 }
      |""".stripMargin)

  @Test def serializableLambdas(): Unit = assertSameClasses(
    """class C extends Serializable {
      |  def f(a: Int) = (b: Int) => (c: Int) => a + b + c
      |  val g: Runnable with Serializable = null
      |}
      |""".stripMargin)

  @Test def controlFlow(): Unit = assertSameClasses(
    """class C {
      |  def f(x: Int) = x match { case 1 => "a"; case 2 | 3 => "b"; case _ if x > 9 => "c"; case _ => throw new Exception }
      |  def g(xs: List[Int]): Int = { var s = 0; var l = xs; while (l.nonEmpty) { s += l.head; l = l.tail }; s }
      |  def h(x: Int): Int = try { if (x > 0) return 1; 2 } finally println()
      |  def i = this.synchronized { println() }
      |  def j(x: AnyRef): Int = { if (x eq null) throw new Exception else 1 }
      |}
      |""".stripMargin)

  /** ASM's stand-in for dead code is valid, and the only way dead code that codegen misses shows. */
  @Test def deadCodeLeftToAsm(): Unit = {
    val code = "class C { def f: Int = { var x = 0; while (true) { x += 1; if (x > 0) return x }; try 1 catch { case _: Exception => 2 } } }"
    val List((file, bytes)) = direct.compileToBytes(code)
    assertEquals("C.class", file)
    val loader = new ClassLoader(null) { def define() = defineClass("C", bytes, 0, bytes.length) }
    assertEquals(1, loader.define().getDeclaredMethods.count(_.getName == "f")) // links, so verifies, the class
  }

  /** A string constant that is too long throws during emission, and generating the class again reports as ever. */
  @Test def errorsFromTheTreePath(): Unit = {
    val code = s"""class C { def f = "${"x" * 70000}" }"""
    def errors(c: Compiler) = {
      val run = new c.global.Run
      run.compileSources(List(makeSourceFile(code, "unitTestSource.scala")))
      val infos = c.global.reporter.asInstanceOf[StoreReporter].infos.toList.map(_.msg)
      c.resetOutput()
      c.global.reporter.reset()
      infos
    }
    val fromTree = errors(tree)
    assertTrue(fromTree.toString, fromTree.exists(_.contains("Error while emitting C")))
    assertEquals(fromTree, errors(direct))
  }
}

object DirectEmitTest {
  private lazy val direct = newCompiler()
  private lazy val tree = newCompiler(extraArgs = "-Ybackend-tree")
}
