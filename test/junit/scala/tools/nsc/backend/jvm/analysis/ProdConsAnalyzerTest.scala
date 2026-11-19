package scala.tools.nsc
package backend.jvm
package analysis

import org.junit.Assert._
import org.junit.Test

import scala.tools.asm.Opcodes
import scala.tools.asm.tree.AbstractInsnNode
import scala.tools.nsc.backend.jvm.AsmUtils._
import scala.tools.testkit.ASMConverters._
import scala.tools.testkit.BytecodeTesting
import scala.tools.testkit.BytecodeTesting._

class ProdConsAnalyzerTest extends BytecodeTesting {
  override def compilerArgs = "-opt:l:none"
  import compiler._

  def prodToString(producer: AbstractInsnNode) = producer match {
    case p: InitialProducer => p.toString
    case p => textify(p)
  }

  def testSingleInsn(singletonInsns: Iterable[AbstractInsnNode], expected: String): Unit = {
    testInsn(single(singletonInsns), expected)
  }

  def testMultiInsns(insns: Iterable[AbstractInsnNode], expected: Iterable[String]): Unit = {
    assertTrue(s"Sizes don't match: ${insns.size} vs ${expected.size}", insns.size == expected.size)
    for (insn <- insns) {
      val txt = prodToString(insn)
      assertTrue(s"Instruction $txt not found in ${expected mkString ", "}", expected.exists(txt.contains))
    }
  }

  def testInsn(insn: AbstractInsnNode, expected: String): Unit = {
    val txt = prodToString(insn)
    assertTrue(s"Expected $expected, found $txt", txt contains expected)
  }

  def single[T](c: Iterable[T]): T = {
    assertTrue(s"Expected singleton collection, got $c", c.size == 1)
    c.head
  }

  @Test
  def parameters(): Unit = {
    val m = compileAsmMethod("def f = this.toString")
    val a = new ProdConsAnalyzer(m, "C")
    val call = findInstr(m, "INVOKEVIRTUAL")

    testSingleInsn(a.producersForValueAt(call, 1), "ALOAD 0") // producer of stack value
    testSingleInsn(a.producersForInputsOf(call), "ALOAD 0")

    testSingleInsn(a.consumersOfValueAt(call.getNext, 1), "ARETURN")  // consumer of `toString` result
    testSingleInsn(a.consumersOfOutputsFrom(call), "ARETURN")

    testSingleInsn(a.ultimateConsumersOfValueAt(call.getNext, 1), "ARETURN")

    testSingleInsn(a.initialProducersForValueAt(call, 1), "ParameterProducer")
    testSingleInsn(a.producersForValueAt(call, 0), "ParameterProducer")
  }

  @Test
  def parametersInitialProducer(): Unit = {
    // mutates a parameter local (not possible in scala, but in bytecode)
    import Opcodes._
    val m = genMethod(descriptor = "(I)I")(
      Label(0),
      VarOp(ILOAD, 1),
      Jump(IFNE, Label(1)),
      Op(ICONST_1),
      VarOp(ISTORE, 1),
      Label(1),
      VarOp(ILOAD, 1),
      Op(IRETURN),
      Label(2)
    )
    m.maxLocals = 2
    m.maxStack = 1
    val a = new ProdConsAnalyzer(m, "C")

    val ifne = findInstr(m, "IFNE")
    testSingleInsn(a.producersForValueAt(ifne, 1), "ParameterProducer")

    val ret = findInstr(m, "IRETURN")
    testMultiInsns(a.producersForValueAt(ret, 1), List("ParameterProducer", "ISTORE 1"))
  }

  @Test
  def branching(): Unit = {
    val m = compileAsmMethod("def f(x: Int) = { var a = x; if (a == 0) a = 12; a }")
    val a = new ProdConsAnalyzer(m, "C")

    val ret = findInstr(m, "IRETURN")
    testMultiInsns(a.producersForValueAt(ret, 2), List("ISTORE 2", "ISTORE 2"))
    testMultiInsns(a.initialProducersForValueAt(ret, 2), List("BIPUSH 12", "ParameterProducer"))

    val bipush = findInstr(m, "BIPUSH 12")
    testSingleInsn(a.consumersOfOutputsFrom(bipush), "ISTORE 2")
    testSingleInsn(a.ultimateConsumersOfValueAt(bipush.getNext, 3), "IRETURN")
  }

  @Test
  def checkCast(): Unit = {
    val m = compileAsmMethod("def f(o: Object) = o.asInstanceOf[String]")
    val a = new ProdConsAnalyzer(m, "C")
    assert(findInstrs(m, "CHECKCAST java/lang/String").length == 1)

    val ret = findInstr(m, "ARETURN")
    testSingleInsn(a.initialProducersForInputsOf(ret), "ParameterProducer(1)")
  }

  @Test
  def instanceOf(): Unit = {
    val m = compileAsmMethod("def f(o: Object) = o.isInstanceOf[String]")
    val a = new ProdConsAnalyzer(m, "C")
    assert(findInstrs(m, "INSTANCEOF java/lang/String").length == 1)

    val ret = findInstr(m, "IRETURN")
    testSingleInsn(a.initialProducersForInputsOf(ret), "INSTANCEOF")
  }

  @Test
  def unInitLocal(): Unit = {
    val m = compileAsmMethod("def f(b: Boolean) = { if (b) { var a = 0; println(a) }; 1 }")
    val a = new ProdConsAnalyzer(m, "C")

    val store = findInstr(m, "ISTORE")
    val call  = findInstr(m, "INVOKEVIRTUAL")
    val ret   = findInstr(m, "IRETURN")

    testSingleInsn(a.producersForValueAt(store, 2), "UninitializedLocalProducer(2)")
    testSingleInsn(a.producersForValueAt(call, 2), "ISTORE")
    testMultiInsns(a.producersForValueAt(ret, 2), List("UninitializedLocalProducer", "ISTORE"))
  }

  @Test
  def dupCopying(): Unit = {
    val m = compileAsmMethod("def f = new Object")
    val a = new ProdConsAnalyzer(m, "C")

    val newO   = findInstr(m, "NEW")
    val constr = findInstr(m, "INVOKESPECIAL")

    testSingleInsn(a.producersForInputsOf(constr), "DUP")
    testSingleInsn(a.initialProducersForInputsOf(constr), "NEW")

    testSingleInsn(a.consumersOfOutputsFrom(newO), "DUP")
    testMultiInsns(a.ultimateConsumersOfOutputsFrom(newO), List("INVOKESPECIAL", "ARETURN"))
  }

  @Test
  def multiProducer(): Unit = {
    import Opcodes._
    val m = genMethod(descriptor = "(I)I")(
      VarOp(ILOAD, 1),
      VarOp(ILOAD, 1),
      Op(DUP2),
      Op(IADD),
      Op(SWAP),
      VarOp(ISTORE, 1),
      Op(IRETURN)
    )
    m.maxLocals = 2
    m.maxStack = 4
    val a = new ProdConsAnalyzer(m, "C")

    val dup2  = findInstr(m, "DUP2")
    val add   = findInstr(m, "IADD")
    val swap  = findInstr(m, "SWAP")
    val store = findInstr(m, "ISTORE")
    val ret   = findInstr(m, "IRETURN")

    testMultiInsns(a.producersForInputsOf(dup2), List("ILOAD", "ILOAD"))
    testSingleInsn(a.consumersOfValueAt(dup2.getNext, 4), "IADD")
    testSingleInsn(a.consumersOfValueAt(dup2.getNext, 5), "IADD")
    testMultiInsns(a.consumersOfOutputsFrom(dup2), List("IADD", "SWAP"))

    testSingleInsn(a.ultimateConsumersOfOutputsFrom(dup2), "IADD") // the 'store' is not here: it's a copying instr, so not an ultimate consumer.
    testMultiInsns(a.consumersOfOutputsFrom(swap), List("IRETURN", "ISTORE"))
    testSingleInsn(a.ultimateConsumersOfOutputsFrom(swap), "IRETURN") // again, no store
    testSingleInsn(a.initialProducersForInputsOf(add), "ParameterProducer(1)")

    testMultiInsns(a.producersForInputsOf(swap), List("IADD", "DUP2"))
    testSingleInsn(a.consumersOfValueAt(swap.getNext, 4), "ISTORE")
    testSingleInsn(a.consumersOfValueAt(swap.getNext, 3), "IRETURN")
    testSingleInsn(a.initialProducersForInputsOf(store), "ParameterProducer(1)")
    testSingleInsn(a.initialProducersForInputsOf(ret), "IADD")
  }

  @Test
  def iincProdCons(): Unit = {
    import Opcodes._
    val m = genMethod(descriptor = "(I)I")(
      Incr(IINC, 1, 1), // producer and consumer of local variable 1
      VarOp(ILOAD, 1),
      Op(IRETURN)
    )
    m.maxLocals = 2
    m.maxStack = 1
    val a = new ProdConsAnalyzer(m, "C")

    val inc = findInstr(m, "IINC")
    val load = findInstr(m, "ILOAD")
    val ret = findInstr(m, "IRETURN")

    testSingleInsn(a.producersForInputsOf(inc), "ParameterProducer(1)")
    testSingleInsn(a.consumersOfOutputsFrom(inc), "ILOAD")
    testSingleInsn(a.ultimateConsumersOfOutputsFrom(inc), "IRETURN")
    testSingleInsn(a.consumersOfValueAt(inc, 1), "IINC") // parameter value has a single consumer, the IINC
    testSingleInsn(a.ultimateConsumersOfValueAt(inc, 1), "IINC")

    testSingleInsn(a.producersForInputsOf(load), "IINC")
    testSingleInsn(a.producersForValueAt(load, 1), "IINC")

    testSingleInsn(a.initialProducersForInputsOf(ret), "IINC")
  }

  @Test
  def copyingInsns(): Unit = {
    val m = compileAsmMethod("def f = 0L.asInstanceOf[Int]")
    val a = new ProdConsAnalyzer(m, "C")

    val cnst = findInstr(m, "LCONST_0")
    val l2i  = findInstr(m, "L2I") // l2i is not a copying instruction
    val ret  = findInstr(m, "IRETURN")

    testSingleInsn(a.consumersOfOutputsFrom(cnst), "L2I")
    testSingleInsn(a.ultimateConsumersOfOutputsFrom(cnst), "L2I")

    testSingleInsn(a.producersForInputsOf(l2i), "LCONST_0")
    testSingleInsn(a.initialProducersForInputsOf(l2i), "LCONST_0")

    testSingleInsn(a.consumersOfOutputsFrom(l2i), "IRETURN")
    testSingleInsn(a.producersForInputsOf(ret), "L2I")
  }

  @Test
  def cyclicProdCons(): Unit = {
    import Opcodes._
    val m = genMethod(descriptor = "(I)I")(
      Label(1),
      VarOp(ILOAD, 1),
      IntOp(BIPUSH, 10),
      Op(IADD),          // consumer of the above ILOAD

      Op(ICONST_0),
      Jump(IF_ICMPNE, Label(2)),

      VarOp(ILOAD, 1),
      VarOp(ISTORE, 1),
      Jump(GOTO, Label(1)),

      Label(2),
      IntOp(BIPUSH, 9),
      Op(IRETURN)
    )
    m.maxLocals = 2
    m.maxStack = 2
    val a = new ProdConsAnalyzer(m, "C")

    val iadd = findInstr(m, "IADD")
    val firstLoad = iadd.getPrevious.getPrevious
    assert(firstLoad.getOpcode == ILOAD)
    val secondLoad = findInstr(m, "ISTORE").getPrevious
    assert(secondLoad.getOpcode == ILOAD)

    testSingleInsn(a.producersForValueAt(iadd, 2), "ILOAD")
    testSingleInsn(a.initialProducersForValueAt(iadd, 2), "ParameterProducer(1)")
    testMultiInsns(a.producersForInputsOf(firstLoad), List("ParameterProducer", "ISTORE"))
    testMultiInsns(a.producersForInputsOf(secondLoad), List("ParameterProducer", "ISTORE"))

    testSingleInsn(a.ultimateConsumersOfOutputsFrom(firstLoad), "IADD")
    testSingleInsn(a.ultimateConsumersOfOutputsFrom(secondLoad), "IADD")

    testSingleInsn(a.consumersOfOutputsFrom(firstLoad), "IADD")
    testSingleInsn(a.consumersOfOutputsFrom(secondLoad), "ISTORE")
  }

  // `t = x; x = y; y = t` in a loop: the copy operations form a cycle through three locals
  def swapLoop() = {
    import Opcodes._
    val m = genMethod(descriptor = "(II)I")(
      Label(1),
      VarOp(ILOAD, 1),
      VarOp(ISTORE, 3),  // t = x
      VarOp(ILOAD, 2),
      VarOp(ISTORE, 1),  // x = y
      VarOp(ILOAD, 3),
      VarOp(ISTORE, 2),  // y = t
      VarOp(ILOAD, 1),
      Jump(IFNE, Label(1)),
      VarOp(ILOAD, 2),
      Op(IRETURN)
    )
    m.maxLocals = 4
    m.maxStack = 1
    m
  }

  @Test
  def cyclicCopiesInitialProducers(): Unit = {
    val m = swapLoop()
    val storeX = findInstrs(m, "ISTORE 1").head
    val ret = findInstr(m, "IRETURN")
    val expected = List("ParameterProducer(1)", "ParameterProducer(2)")

    testMultiInsns(new ProdConsAnalyzer(m, "C").initialProducersForInputsOf(storeX), expected)

    val a = new ProdConsAnalyzer(m, "C")
    testMultiInsns(a.initialProducersForInputsOf(ret), expected)
    testMultiInsns(a.initialProducersForInputsOf(storeX), expected)

    assertQueryOrderIndependent(m, _.initialProducersForInputsOf(_))
  }

  @Test
  def cyclicCopiesUltimateConsumers(): Unit = {
    val m = swapLoop()
    val List(loadX, _) = findInstrs(m, "ILOAD 1"): @unchecked
    val loadY = findInstrs(m, "ILOAD 2").head
    val expected = List("IFNE", "IRETURN")

    testMultiInsns(new ProdConsAnalyzer(m, "C").ultimateConsumersOfOutputsFrom(loadY), expected)

    val a = new ProdConsAnalyzer(m, "C")
    testMultiInsns(a.ultimateConsumersOfOutputsFrom(loadX), expected)
    testMultiInsns(a.ultimateConsumersOfOutputsFrom(loadY), expected)

    assertQueryOrderIndependent(m, _.ultimateConsumersOfOutputsFrom(_), _.getOpcode != Opcodes.IRETURN)
  }

  // the answer for every instruction is the same from a fresh analyzer as from one that has already answered queries
  // for the other instructions, in either order
  def assertQueryOrderIndependent(m: scala.tools.asm.tree.MethodNode, query: (ProdConsAnalyzer, AbstractInsnNode) => Set[AbstractInsnNode], include: AbstractInsnNode => Boolean = _ => true): Unit = {
    val insns = m.instructions.toArray.toList.filter(i => i.getOpcode >= 0 && include(i))
    val fresh = insns.map(i => query(new ProdConsAnalyzer(m, "C"), i))
    for (order <- List(insns.indices.toList, insns.indices.reverse.toList)) {
      val a = new ProdConsAnalyzer(m, "C")
      for (i <- order)
        assertEquals(s"${textify(insns(i))} queried in order $order", fresh(i), query(a, insns(i)))
    }
  }
}
