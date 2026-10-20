package scala.reflect.internal

import org.junit.Assert._
import org.junit.Test

import scala.tools.nsc.symtab.SymbolTableForUnitTesting

class ScopeTest {
  object symbolTable extends SymbolTableForUnitTesting

  import symbolTable._

  @Test
  def testNestedScopeSmall(): Unit = testNestedScope(0)
  @Test
  def testNestedScopeLarge(): Unit = testNestedScope(64) // exceeding MIN_HASH

  private def testNestedScope(initSize: Int): Unit = {
    def sym(termName: String): Symbol = NoSymbol.newValue(TermName(termName))
    val foo = sym("foo")
    val bar = sym("bar")

    val outerElems = List.tabulate(initSize)(i => sym(i.toString))
    val outer = newScopeWith(outerElems ++ List(foo, bar) : _*)
    assertTrue(outer.containsName(foo.name))
    assertTrue(outer.containsName(bar.name))

    val baz = sym("baz")
    val nested = newNestedScope(outer)

    // Entries from the outer scope are entered in the nested.
    assertTrue(outer.containsName(foo.name))
    assertTrue(outer.containsName(bar.name))

    // Nested scopes structurally share ScopeEntry-s with the outer.
    assertSame(outer.lookupEntry(foo.name), nested.lookupEntry(foo.name))
    nested.enter(baz)

    // Symbols entered in the nested scope aren't visible in the outer.
    assertTrue(nested.containsName(baz.name))
    assertTrue(!outer.containsName(baz.name))

    // Unlinking a symbol in the inner scope doesn't modify the outer
    nested.unlink(bar)
    assertFalse(nested.containsName(bar.name))
    assertTrue(outer.containsName(bar.name))
  }

  @Test
  def testForeach(): Unit = {
    def sym(termName: String): Symbol = NoSymbol.newValue(TermName(termName))
    for (size <- List(0, 1, 63, 64, 65, 200)) {
      val syms = List.tabulate(size)(i => sym(i.toString))
      val scope = newScopeWith(syms: _*)
      val buf = List.newBuilder[Symbol]
      // mutation during iteration is not observed
      scope.foreach { s => buf += s; scope.enter(sym("x")); scope.unlink(s) }
      assertEquals(syms, buf.result())
      assertEquals(size, scope.toList.count(_.name.toString == "x"))
    }
  }

  @Test
  def testForeachCached(): Unit = {
    def sym(termName: String): Symbol = NoSymbol.newValue(TermName(termName))
    for (size <- List(0, 1, 64, 65, 200)) {
      val syms = List.tabulate(size)(i => sym(i.toString))
      val scope = newScopeWith(syms: _*)
      val cached = scope.toList
      val buf = List.newBuilder[Symbol]
      scope.foreach { s => buf += s; scope.enter(sym("x")) }
      assertEquals(syms, buf.result())
      if (size > 0) assertNotSame(cached, scope.toList)
    }
  }

  @Test
  def testForeachNested(): Unit = {
    def sym(termName: String): Symbol = NoSymbol.newValue(TermName(termName))
    for (outerSize <- List(0, 1, 64, 65); innerSize <- List(0, 1, 64, 65)) {
      val outerSyms = List.tabulate(outerSize)(i => sym("o" + i))
      val innerSyms = List.tabulate(innerSize)(i => sym("i" + i))
      val outer = newScopeWith(outerSyms: _*)
      val nested = newNestedScope(outer)
      innerSyms.foreach(nested.enter(_))
      val buf = List.newBuilder[Symbol]
      nested.foreach(buf += _)
      assertEquals(nested.toList, buf.result())
      val outerBuf = List.newBuilder[Symbol]
      outer.foreach(outerBuf += _)
      assertEquals(outerSyms, outerBuf.result())
    }
  }

  @Test
  def testForeachRepeated(): Unit = {
    def sym(termName: String): Symbol = NoSymbol.newValue(TermName(termName))
    def collect(scope: Scope): List[Symbol] = { val buf = List.newBuilder[Symbol]; scope.foreach(buf += _); buf.result() }
    for (size <- List(0, 1, 64, 65, 200)) {
      val syms = List.tabulate(size)(i => sym(i.toString))
      val scope = newScopeWith(syms: _*)
      assertEquals(syms, collect(scope))
      assertEquals(syms, collect(scope))
      scope.toList
      assertEquals(syms, collect(scope)) // uses elemsCache
      val extra = sym("extra")
      scope.enter(extra)
      assertEquals(size + 1, scope.size)
      assertEquals(syms :+ extra, collect(scope)) // size known
      scope.unlink(extra)
      assertEquals(syms, collect(scope))
      assertEquals(syms, scope.toList)
    }
  }
}
