import scala.tools.partest.BytecodeTest

import scala.tools.nsc.util.JavaClassPath
import java.io.InputStream
import scala.tools.asm
import asm.ClassReader
import asm.tree.{ClassNode, InsnList}
import scala.collection.JavaConverters._

object Test extends BytecodeTest {
  def show: Unit = {
    val classNode = loadClassNode("Foo_1")
    val methodNode = getMethod(classNode, "foo")
    println(countInstanceChecks(methodNode.instructions))
  }

  def countInstanceChecks(insnList: InsnList): Int = {
    def isInstanceOf(node: asm.tree.AbstractInsnNode): Boolean = {
      val opcode = node.getOpcode
      (opcode == asm.Opcodes.INSTANCEOF)
    }
    insnList.iterator.asScala.count(isInstanceOf)
  }
}
