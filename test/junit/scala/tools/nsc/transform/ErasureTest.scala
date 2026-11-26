package scala.tools.nsc.transform

import org.junit.Assert.assertEquals
import org.junit.Test

import scala.tools.nsc.symtab.SymbolTableForUnitTesting
import scala.tools.testkit.BytecodeTesting
import scala.tools.asm.Opcodes._
import scala.tools.testkit.ASMConverters._
import scala.tools.testkit.BytecodeTesting.{assertNoInvoke, assertSameCode, assertSameSummary, getMethod}

class ErasureTest extends BytecodeTesting {
  object symbolTable extends SymbolTableForUnitTesting

  @Test def testGenericArray(): Unit = {
    import symbolTable._
    import definitions._
    val T = NoSymbol.newTypeParameter(TypeName("T")).setInfo(TypeBounds.empty)
    val U = NoSymbol.newTypeParameter(TypeName("U")).setInfo(TypeBounds.empty)
    val arrayTWithArrayString: Type = refinedType(appliedType(ArrayClass, T.tpeHK) :: appliedType(ArrayClass, StringTpe) :: Nil, NoSymbol)
    val arrayTWithArrayU: Type = refinedType(appliedType(ArrayClass, T.tpeHK) :: appliedType(ArrayClass, U.tpeHK) :: Nil, NoSymbol)

    assertEquals(1, erasure.unboundedGenericArrayLevel(appliedType(ArrayClass, T.tpeHK)))
    assertEquals(0, erasure.unboundedGenericArrayLevel(arrayTWithArrayString))
    assertEquals(1, erasure.unboundedGenericArrayLevel(arrayTWithArrayU))
  }

  import compiler._

  @Test
  def noScalaRuntimeForRefinedTypeWithGenericAndConcreteArray(): Unit = {
    val code =
      """class C {
        |  def t[T](xs: Array[T]): Unit = xs match {
        |    case x: Array[AnyRef] => x(0)
        |    case _ =>
        |  }
        |}
      """.stripMargin
    val c = compileClass(code)
    assertNoInvoke(getMethod(c, "t"))
  }

  @Test
  def valueClassOverConstantIsUnboxed(): Unit = {
    val code =
      """class Meters(val value: Double) extends AnyVal
        |class C {
        |  def value = new Meters(7).value
        |  def eq(m: Meters) = new Meters(0) == m
        |}
      """.stripMargin
    val c = compileClasses(code).find(_.name == "C").get
    assertSameCode(getMethod(c, "value"), List(Ldc(LDC, 7.0), Op(DRETURN)))
    assertSameSummary(getMethod(c, "eq"), List(DCONST_0, DLOAD, DCMPL, IFNE, ICONST_1, GOTO, -1, ICONST_0, -1, IRETURN))
  }

  @Test
  def valueClassConditionalReceiverIsUnboxed(): Unit = {
    val code =
      """class Meters(val value: Double) extends AnyVal
        |class C {
        |  def cond(ok: Boolean, d: Double) = (if (ok) new Meters(d) else new Meters(0)).value
        |}
      """.stripMargin
    val c = compileClasses(code).find(_.name == "C").get
    assertSameSummary(getMethod(c, "cond"), List(ILOAD, IFEQ, DLOAD, DRETURN, -1, DCONST_0, DRETURN))
  }
}
