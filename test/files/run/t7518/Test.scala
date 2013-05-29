import scala.tools.partest.BytecodeTest
import scala.tools.asm.util._
import java.io._

object Test extends BytecodeTest {
  def show {
    val classNode = loadClassNode("InlinePosition", skipDebugInfo = false)
    val meth = getMethod(classNode, "client")
    val textifier = new Textifier()
    meth.accept(new TraceMethodVisitor(textifier))
  }
}
