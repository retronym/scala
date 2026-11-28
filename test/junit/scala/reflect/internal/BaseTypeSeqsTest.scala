package scala.reflect.internal

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.tools.nsc.symtab.SymbolTableForUnitTesting

@RunWith(classOf[JUnit4])
class BaseTypeSeqsTest {
  object symbolTable extends SymbolTableForUnitTesting {
    /** As if the `Int` counter of symbol ids had wrapped around and come back to `id`. */
    def nextIdWillBe(id: Int): Unit = ids = id - 1
  }
  import symbolTable._, definitions._

  /** In a long-lived `Global`, symbol ids repeat after about four billion symbols, including the ids of symbols that
   *  live as long as the `Global`, such as `AnyClass`, which every base type sequence contains.
   */
  @Test def baseTypeIndexComparesSymbolsNotIds(): Unit = {
    def cls(name: String): ClassSymbol = {
      val c = rootMirror.RootClass.newClassSymbol(TypeName(name), NoPosition, 0L)
      c.setInfo(ClassInfoType(List(AnyRefTpe), newScope, c))
      c
    }
    val c = cls("C")
    symbolTable.nextIdWillBe(AnyClass.id)
    val d = cls("D")
    assertEquals(AnyClass.id, d.id)

    assertTrue(c.isSubClass(AnyClass))
    assertEquals(-1, c.info.baseTypeIndex(d))
    assertFalse(c.isSubClass(d))
    assertFalse(c.tpe <:< d.tpe)
  }

  @Test def compoundBaseTypeSeqMergesParents(): Unit = {
    val IterableClass = rootMirror.getRequiredClass("scala.collection.Iterable")
    val SetClass = rootMirror.getRequiredClass("scala.collection.immutable.Set")
    val listInt = appliedType(ListClass, IntTpe)
    val setString = appliedType(SetClass, StringTpe)
    val tp = refinedType(List(listInt, setString), NoSymbol)
    val bts = tp.baseTypeSeq
    val syms = bts.toList.map(_.typeSymbol)

    assertEquals(tp.typeSymbol, syms.head)
    assertEquals(AnyClass, syms.last)
    // the base classes of both parents, each once, in the order of `isLess`
    assertEquals((listInt.baseClasses ++ setString.baseClasses).distinct.toSet, syms.tail.toSet)
    assertEquals(syms.distinct, syms)
    assertTrue(syms.tail.zip(syms.tail.tail).forall { case (a, b) => !(b isLess a) })
    // a base class of both parents, at different type arguments, has their intersection
    bts.rawElem(syms.indexOf(IterableClass)) match {
      case RefinedType(ps, _) =>
        assertEquals(Set(appliedType(IterableClass, IntTpe), appliedType(IterableClass, StringTpe)).map(_.toString), ps.map(_.toString).toSet)
      case other => fail(s"expected an intersection, got $other")
    }
    // and a base class of one parent only has that parent's base type
    assertTrue(bts.rawElem(syms.indexOf(ListClass)) =:= listInt)
    // with no parents, the sequence is the type alone
    val empty = refinedType(Nil, NoSymbol)
    assertEquals(List(empty.typeSymbol), empty.baseTypeSeq.toList.map(_.typeSymbol))
  }
}
