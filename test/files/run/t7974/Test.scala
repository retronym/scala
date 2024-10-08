//> using options -Xcheckinit:false
import java.io.PrintWriter

import scala.tools.partest.BytecodeTest
import scala.tools.nsc.backend.jvm.AsmUtils
import scala.tools.asm.util._
import scala.tools.nsc.util.stringFromWriter
import scala.jdk.CollectionConverters._

object Test extends BytecodeTest {
  def show(): Unit = {
    val classNode = loadClassNode("Symbols", skipDebugInfo = true)
    classNode.methods.asScala.foreach(m => println(AsmUtils.textify(m)))
  }
}
