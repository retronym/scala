package scala.tools.nsc.transform

import org.junit.Assert.assertEquals
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.jdk.CollectionConverters._
import scala.tools.testkit.BytecodeTesting
import scala.tools.testkit.BytecodeTesting._

@RunWith(classOf[JUnit4])
class ReleaseFenceTest extends BytecodeTesting {
  import compiler._

  @Test
  def finalValInParentTraitCausesReleaseFenceInSubclassConstructor(): Unit = {
    val code =
      """class C extends T { println(""); object Inner extends T { println("") }}; object O extends T { println("") }; trait T { final val x: String = "" } """
    val classes = compileClasses(code)
    def check(className: String, hasFence: Boolean): Unit = {
      val cnode = classes.find(_.name == className).get
      if (hasFence)
        assertInvoke(getMethod(cnode, "<init>"), "scala/runtime/Statics", "releaseFence")
      else
        assertDoesNotInvoke(getMethod(cnode, "<init>"), "releaseFence")
    }
    check("C", hasFence = true)
    check("C$Inner$", hasFence = true)
    // top level module initialization is guarded within the <clinit> lock, we don't need to emit releaseFence
    check("O$", hasFence = false)
  }

  @Test
  def localCapturedInTraitCausesReleaseFenceInEveryClassWithTheField(): Unit = {
    val code =
      """class F {
        |  def f(x: Int) = {
        |    trait LT { def g = x }
        |    trait LT2 extends LT
        |    class C1 extends LT
        |    class C2 extends LT2
        |    class C3 extends C1
        |    List[Any](new C1, new C2, new C3, new LT2 {})
        |  }
        |}""".stripMargin
    val classes = compileClasses(code)
    def check(className: String, hasField: Boolean): Unit = {
      val cnode = classes.find(_.name == className).get
      assertEquals(className, hasField, cnode.fields.asScala.exists(_.name.startsWith("x$")))
      if (hasField)
        assertInvoke(getMethod(cnode, "<init>"), "scala/runtime/Statics", "releaseFence")
      else
        assertDoesNotInvoke(getMethod(cnode, "<init>"), "releaseFence")
    }
    // the non-final field for the captured `x` is added to each class that mixes in LT
    check("F$C1$1", hasField = true)
    check("F$C2$1", hasField = true)
    check("F$$anon$1", hasField = true)
    // inherits the field from C1
    check("F$C3$1", hasField = false)
  }
}
