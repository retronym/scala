package scala.tools.nsc
package backend.jvm
package opt

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.tools.asm.Opcodes._
import scala.tools.asm.tree.{InsnNode, MethodNode}
import scala.tools.asm.tree.analysis._
import scala.tools.nsc.backend.jvm.analysis.{AliasingAnalyzer, AliasingFrame}
import scala.tools.nsc.backend.jvm.opt.BytecodeUtils._
import scala.tools.testkit.BytecodeTesting
import scala.tools.testkit.BytecodeTesting._

@RunWith(classOf[JUnit4])
class AnalyzerTest extends BytecodeTesting {
  override def compilerArgs = "-opt:l:none"
  import compiler._

  @Test
  def aliasingOfPrimitives(): Unit = {
    val code =
      """class C {
        |  def f(a: Int, b: Long) = {
        |    val c = a - b // a is converted with i2l
        |    val d = c
        |    val e = a
        |    // locals: 0 this -- 1 a -- 2-3 b -- 4-5 c -- 6-7 d -- 8 e
        |    e + d // e is converted with i2l
        |  }
        |}
      """.stripMargin

    val c = compileClass(code)
    val a = new AliasingAnalyzer(new BasicInterpreter)
    val f = getAsmMethod(c, "f")
    a.analyze("C", f)

    val List(_, i2l) = findInstrs(f, "I2L")
    val aliasesAtI2l = a.frameAt(i2l, f).asInstanceOf[AliasingFrame[_]].aliases
    assertEquals(aliasesAtI2l(1).iterator.toList, List(1, 8, 9)) // a, e and stack top
    assertEquals(aliasesAtI2l(4).iterator.toList, List(4, 6))

    val add = findInstr(f, "LADD")
    val aliasesAtAdd = a.frameAt(add, f).asInstanceOf[AliasingFrame[_]].aliases
    assertEquals(aliasesAtAdd(1).iterator.toList, List(1, 8)) // after i2l the value on the stack is no longer an alias
    assertEquals(aliasesAtAdd(4).iterator.toList, List(4, 6, 10)) // c, d and stack top
  }

  /**
   * Checks the alias sets after a stack manipulation instruction.
   *
   * A static method with one parameter per character in `params` (`I` or `J`), preceded by an extra
   * `I` parameter, loads all its parameters and then executes `op`. The extra parameter sits below
   * the values touched by the instruction and shows up as value 0 at the bottom of the stack.
   *
   * `expected` lists the stack after `op`, bottom to top, as indices into `params`. Every stack
   * value has to be aliased with the parameter it holds and with the other copies of it.
   */
  private def checkStackAliases(op: Int, params: String, expected: Int*): Unit = {
    val ps = "I" + params
    def size(p: Int) = if (ps(p) == 'J') 2 else 1
    val local = ps.indices.map(p => (0 until p).map(size).sum)
    val stack = 0 +: expected.map(_ + 1)

    val m = new MethodNode(ACC_STATIC, "m", s"($ps)V", null, null)
    for (p <- ps.indices) m.visitVarInsn(if (size(p) == 2) LLOAD else ILOAD, local(p))
    m.visitInsn(op)
    val after = new InsnNode(NOP)
    m.instructions.add(after)
    for (p <- stack.reverse) m.visitInsn(if (size(p) == 2) POP2 else POP)
    m.visitInsn(RETURN)
    m.maxLocals = ps.indices.map(size).sum
    m.maxStack = stack.map(size).sum

    val a = new AliasingAnalyzer(new BasicInterpreter)
    a.analyze("C", m)
    val frame = a.frameAt(after, m).asInstanceOf[AliasingFrame[_]]
    assertEquals(stack.length, frame.getStackSize)
    def aliases(value: Int) = frame.aliasesOf(value).iterator.toList.sorted
    for (p <- ps.indices) {
      // a category 2 value takes a single entry on the frame's stack
      val copies = local(p) :: stack.indices.filter(stack(_) == p).map(m.maxLocals + _).toList
      for (value <- copies)
        assertEquals(s"aliases of value $value, a copy of parameter $p", copies, aliases(value))
    }
  }

  @Test def aliasingDup(): Unit         = checkStackAliases(DUP, "I", 0, 0)
  @Test def aliasingDupX1(): Unit       = checkStackAliases(DUP_X1, "II", 1, 0, 1)
  @Test def aliasingDupX2Form1(): Unit  = checkStackAliases(DUP_X2, "III", 2, 0, 1, 2)
  @Test def aliasingDupX2Form2(): Unit  = checkStackAliases(DUP_X2, "JI", 1, 0, 1)
  @Test def aliasingDup2Form1(): Unit   = checkStackAliases(DUP2, "II", 0, 1, 0, 1)
  @Test def aliasingDup2Form2(): Unit   = checkStackAliases(DUP2, "J", 0, 0)
  @Test def aliasingDup2X1Form1(): Unit = checkStackAliases(DUP2_X1, "III", 1, 2, 0, 1, 2)
  @Test def aliasingDup2X1Form2(): Unit = checkStackAliases(DUP2_X1, "IJ", 1, 0, 1)
  @Test def aliasingDup2X2Form1(): Unit = checkStackAliases(DUP2_X2, "IIII", 2, 3, 0, 1, 2, 3)
  @Test def aliasingDup2X2Form2(): Unit = checkStackAliases(DUP2_X2, "IIJ", 2, 0, 1, 2)
  @Test def aliasingDup2X2Form3(): Unit = checkStackAliases(DUP2_X2, "JII", 1, 2, 0, 1, 2)
  @Test def aliasingDup2X2Form4(): Unit = checkStackAliases(DUP2_X2, "JJ", 1, 0, 1)
  @Test def aliasingSwap(): Unit        = checkStackAliases(SWAP, "II", 1, 0)
}
