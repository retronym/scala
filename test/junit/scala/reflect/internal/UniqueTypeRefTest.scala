package scala.reflect.internal

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.tools.nsc.symtab.SymbolTableForUnitTesting
import scala.tools.testkit.AllocationTest

@RunWith(classOf[JUnit4])
class UniqueTypeRefTest extends AllocationTest {
  object symbolTable extends SymbolTableForUnitTesting
  import symbolTable._, definitions._

  @Test
  def existingTypeRefIsFoundWithoutAllocating(): Unit = {
    val pre = ListClass.typeConstructor.prefix
    val listOfInt = TypeRef(pre, ListClass, List(IntTpe))
    // equal but not identical components
    val args = List(TypeRef(IntTpe.prefix, IntClass, Nil))
    assertSame(listOfInt, TypeRef(pre, ListClass, args))
    assertSame(listOfInt, nonAllocating(TypeRef(pre, ListClass, args)))
    assertSame(StringTpe, nonAllocating(TypeRef(StringTpe.prefix, StringClass, Nil)))
  }

  @Test
  def missAddsTheNewTypeRef(): Unit = {
    val tparam = ListClass.newTypeParameter(newTypeName("UniqueTypeRefTest_T"))
    tparam.setInfo(TypeBounds.empty)
    val tp = TypeRef(NoPrefix, tparam, Nil)
    assertSame(tp, TypeRef(NoPrefix, tparam, Nil))
    val applied = TypeRef(ListClass.typeConstructor.prefix, ListClass, List(tp))
    assertSame(applied, TypeRef(ListClass.typeConstructor.prefix, ListClass, List(tp)))
  }

  @Test
  def javaObjectIsNotUnique(): Unit = {
    // `ObjectTpeJava` is equal in its components to `ObjectTpe`, but must not be returned in its place.
    assertNotSame(ObjectTpeJava, ObjectTpe)
    assertSame(ObjectTpe, TypeRef(ObjectTpe.prefix, ObjectClass, Nil))
  }
}
