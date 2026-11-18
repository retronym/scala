package scala.reflect.internal

import org.junit.Assert._
import org.junit.Test

import scala.tools.nsc.symtab.SymbolTableForUnitTesting

class SymbolsTest {
  object symbolTable extends SymbolTableForUnitTesting

  import symbolTable._

  @Test
  def sealedSortNameUsesTheUnexpandedName(): Unit = {
    val owner = rootMirror.EmptyPackageClass.newClass(TypeName("C"))
    val sym = owner.newValue(TermName("x"))
    val before = sym.sealedSortName
    sym.expandName(owner)
    assertNotEquals(TermName("x"), sym.rawname)
    assertEquals(before, sym.sealedSortName)
    assertTrue(before, before.startsWith("x#"))
  }
}
