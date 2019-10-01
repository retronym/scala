import scala.tools.partest.BytecodeTest
import scala.tools.asm
import scala.collection.JavaConverters._
import scala.PartialFunction.cond

object Test extends BytecodeTest {
  def show: Unit = {
    val clasz = loadClassNode("Switches")
    List("two", "guard", "colli") foreach { meth =>
      val mn = getMethod(clasz, meth)
      assert(mn.instructions.iterator.asScala.exists(isSwitchInsn), meth)
    }
  }
  def isSwitchInsn(insn: asm.tree.AbstractInsnNode) =
    cond(insn.getOpcode) { case asm.Opcodes.LOOKUPSWITCH | asm.Opcodes.TABLESWITCH => true }
}
