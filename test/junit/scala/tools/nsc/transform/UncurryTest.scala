package scala.tools.nsc
package transform

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.tools.testkit.ASMConverters._
import scala.tools.testkit.BytecodeTesting

@RunWith(classOf[JUnit4])
class UncurryTest {
  import BytecodeTesting._

  @Test def emptyVarargsDoesNotAllocate(): Unit = {
    val code =
      """
        |def test0 = va0()
        |def test1 = va0(0)
        |def test2 = va1(0)
        |def test3 = va1(0, 0)
        |def va0(elems: Int*): Int = ???
        |def va1(first: Int, rest: Int*): Int = ???
        |
      """.stripMargin
    val compiler = newCompiler()
    val List(test0, test1, test2, test3, va0, va1) = compiler.compileMethods(code)
    assertDoesNotInvoke(test0, "wrapIntArray")
    assertInvoke(test1, "scala/runtime/ScalaRunTime$", "wrapIntArray")
    assertDoesNotInvoke(test2, "wrapIntArray")
    assertInvoke(test3, "scala/runtime/ScalaRunTime$", "wrapIntArray")
  }

  @Test def tryLift(): Unit = {
    val code =
      """class C {
        |  private val lock = new Object
        |  def f(s: String): Int = s.length
        |  var field = 0
        |
        |  // not lifted: the body of `synchronized` starts with an empty stack, the monitor is in a local
        |  def syncStat(s: String): Int = {
        |    var count = 0
        |    lock.synchronized { try { count += 1; f(s) } catch { case _: NumberFormatException => count -= 1 } }
        |    count
        |  }
        |  def syncExpr(s: String): Int = lock.synchronized { try f(s) catch { case _: NumberFormatException => 0 } }
        |  def syncVal(s: String): Int = { val r = lock.synchronized { try f(s) catch { case _: NumberFormatException => 0 } }; r + 1 }
        |  def syncNested(s: String): Int = lock.synchronized { lock.synchronized { try f(s) catch { case _: NumberFormatException => 0 } } }
        |  def syncBlock(s: String): Int = lock.synchronized { val r = try f(s) catch { case _: NumberFormatException => 0 }; r + 1 }
        |  def syncQual(s: String): Int = (try lock catch { case _: NumberFormatException => this }).synchronized { f(s) }
        |
        |  // lifted: there is a value on the stack when the `try` is evaluated
        |  def syncArg(s: String): Int = f(lock.synchronized { try s catch { case _: NumberFormatException => "" } })
        |  def syncOperand(s: String): Int = 1 + lock.synchronized { try f(s) catch { case _: NumberFormatException => 0 } }
        |  def syncInArg(s: String): Int = lock.synchronized { f(try s catch { case _: NumberFormatException => "" }) }
        |  def assignField(s: String): Unit = field = try f(s) catch { case _: NumberFormatException => 0 }
        |  def assignLocal(s: String): Int = { var r = 0; r = try f(s) catch { case _: NumberFormatException => 0 }; r }
        |  def assignCaptured(s: String): Int = { var r = 0; r = try f(s) catch { case _: NumberFormatException => 0 }; val g = () => r; g() }
        |  // the receiver of a String concatenation: the StringBuilder (target 8) is on the stack
        |  def concatSync(s: String): String = lock.synchronized { try s catch { case _: NumberFormatException => "" } } + s
        |  def concatTry(s: String): String = (try f(s) catch { case _: NumberFormatException => 0 }).toString + s
        |  def concatIntRecv(s: String): String = (try f(s) catch { case _: NumberFormatException => 0 }) + s
        |  def concatNested(s: String): String = (lock.synchronized { try s catch { case _: NumberFormatException => "" } } + s) + s
        |  def concatArg(s: String): String = s + lock.synchronized { try f(s) catch { case _: NumberFormatException => 0 } }
        |  def arrayStore(s: String): Unit = { val a = new Array[Int](1); a(0) = lock.synchronized { try f(s) catch { case _: NumberFormatException => 0 } } }
        |  def newArg(s: String): Option[String] = new Some(lock.synchronized { try s catch { case _: NumberFormatException => "" } })
        |}
      """.stripMargin
    // Target 9 and later concatenate Strings with `StringConcatFactory`, which the compiler needs on the classpath (JDK 9+).
    val targets = if (scala.util.Properties.isJavaAtLeast("9")) List("8", "17") else List("8")
    for (target <- targets) {
      val c = newCompiler(extraArgs = s"-target:$target -Wconf:cat=deprecation:s").compileClass(code)
      def callsLiftedTree(m: Method) = m.instructions.exists { case i: Invoke => i.name.startsWith("liftedTree") case _ => false }
      for (name <- List("syncStat", "syncExpr", "syncVal", "syncNested", "syncBlock", "syncQual")) {
        val m = getMethod(c, name)
        assertTrue(name, m.handlers.exists(_.desc.contains("java/lang/NumberFormatException")))
        assertFalse(name, callsLiftedTree(m))
        // no IntRef for `count`
        assertFalse(name, m.instructions.exists { case TypeOp(_, desc) => desc.startsWith("scala/runtime/") case _ => false })
      }
      for (name <- List("syncArg", "syncOperand", "syncInArg", "assignField", "assignLocal", "assignCaptured",
                        "concatSync", "concatTry", "concatIntRecv", "concatNested", "concatArg", "arrayStore", "newArg"))
        assertTrue(s"$name -target:$target", callsLiftedTree(getMethod(c, name)))
    }
  }

}
