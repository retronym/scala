package scala.reflect.internal

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import scala.tools.nsc.symtab.SymbolTableForUnitTesting

@RunWith(classOf[JUnit4])
class TypesTest {

  object symbolTable extends SymbolTableForUnitTesting
  import symbolTable._, definitions._

  @Test
  def testRefinedTypeSI8611(): Unit = {
    def stringNarrowed = StringTpe.narrow
    assert(stringNarrowed != stringNarrowed)
    assert(!(stringNarrowed =:= stringNarrowed))

    def boolWithString = refinedType(BooleanTpe :: StringTpe :: Nil, NoSymbol)
    assert(boolWithString != boolWithString)
    assert(boolWithString =:= boolWithString)

    val boolWithString1 = boolWithString
    val boolWithString1narrow1 = boolWithString1.narrow
    val boolWithString1narrow2 = boolWithString1.narrow
    // Two narrowings of the same refinement end up =:=. This was the root
    // cause of SI-8611. See `narrowUniquely` in `Logic` for the workaround.
    assert(boolWithString1narrow1 =:= boolWithString1narrow2)
    val uniquelyNarrowed1 = refinedType(boolWithString1narrow1 :: Nil, NoSymbol)
    val uniquelyNarrowed2 = refinedType(boolWithString1narrow2 :: Nil, NoSymbol)
    assert(uniquelyNarrowed1 =:= uniquelyNarrowed2)
  }

  @Test
  def constantToString(): Unit = {
    def check(value: Any, expected: String) = assertEquals(expected, Constant(value).escapedStringValue)
    check((), "()")
    check(true, "true")
    check(null, "null")
    check("", "\"\"")
    check("foo", "\"foo\"")
    check("\tfoo", "\"\\tfoo\"")
    check("\"", "\"\\\"\"")
    check("\"", "\"\\\"\"")
    check('\t', "'\\t'")
    check('A', "'A'")
    check(1 : Byte, "1")
    check(1 : Short, "1")
    check(1, "1")
    check(1f, "1.0")
    check(1d, "1.0")
    check(Double.NaN, "NaN")
    check(1L, "1L")
    check(typeOf[String], "classOf[java.lang.String]")
  }
}
