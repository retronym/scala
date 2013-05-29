import scala.tools.partest.BytecodeTest
import scala.tools.asm.util._
import scala.tools.nsc.util.stringFromWriter

object Test extends BytecodeTest {
  def show {
    // your code that inspect ASM trees and prints values
    val classNode = loadClassNode("InlinePosition", skipDebugInfo = false)
    val meth = getMethod(classNode, "client")
    println(meth.getClass)
    val textifier = new Textifier()
    meth.accept(new TraceMethodVisitor(textifier))
    println(stringFromWriter(w => textifier.print(w)))
  }
}
