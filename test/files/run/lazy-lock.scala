import scala.tools.partest._
import java.io.{Console => _, _}
import scala.tools.nsc.util.stringFromStream

object Test extends DirectTest {

  override def extraSettings: String = "-usejavacp -Xprint:mixin -Ylazy-lock -d " + testOutput.path

  override def code = """
class C {
  protected val __lazyLock: AnyRef = new {}
  object O
  lazy val LV = ""

  def foo {
    lazy val CfooLV = ""
    object CfooO
  }
}
trait T {
  protected val __lazyLock: AnyRef = new {}
  object O
  lazy val LV = ""
  def foo = { object TfooA; lazy val TfooLV = ""}
}
object Wrap {
  // Lock in a non-top-level trait: motivates the .toInterface call.
  trait T { def __lazyLock: AnyRef = new {}; def foo = { object WrapTfooA; lazy val WrapTfooLV = ""} }
}

"""

  override def show(): Unit = {
    val output = stringFromStream {out =>
      Console.withOut(out) {
        compile()
      }
    }
    val filtered = output.lines.filter(_.contains("synchronized")).mkString("\n")
    println(filtered)
  }
}


trait T {
  private val __lazyLock: AnyRef = new {}
  def foo = { object TfooA; lazy val TfooLV = ""}
}