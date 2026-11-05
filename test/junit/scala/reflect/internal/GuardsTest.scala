package scala.reflect.internal

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.tools.nsc.symtab.SymbolTableForUnitTesting

/** The guards that type operations use against recursion, each with a small recursive function
 *  over a graph of names that uses it the way the type operations do.
 */
@RunWith(classOf[JUnit4])
class GuardsTest {
  object symbolTable extends SymbolTableForUnitTesting
  import symbolTable._, definitions._

  /** Resolves a name through a chain of aliases to a type, as `lub` and `glb` memoize their
   *  results. On a cycle, the alias resolves to `ErrorType`.
   */
  class Resolver(aliases: Map[String, Either[String, Type]]) {
    val guard = new MemoGuard[String]
    var computed = 0

    // The canonical use of a `MemoGuard`: look the key up, and only on a miss record the key as in
    // progress, compute, and record the result. What a cycle means is said here, at the call site.
    def resolve(name: String): Type = {
      val memo = guard.lookup(name, onCycle = ErrorType)
      if (memo ne null) memo
      else {
        guard.begin(name)
        computed += 1
        val res = aliases(name) match {
          case Left(target) => resolve(target)
          case Right(tpe)   => tpe
        }
        guard.end(name, res)
        res
      }
    }
  }

  @Test def memoGuardMemoizes(): Unit = {
    val r = new Resolver(Map("a" -> Left("b"), "b" -> Left("c"), "c" -> Right(IntTpe)))
    assertEquals(IntTpe, r.resolve("a"))
    assertEquals(3, r.computed)
    assertEquals(IntTpe, r.resolve("b"))
    assertEquals("each name is computed once", 3, r.computed)
  }

  @Test def memoGuardAnswersCyclesAtTheCallSite(): Unit = {
    val r = new Resolver(Map("a" -> Left("b"), "b" -> Left("a")))
    assertEquals(ErrorType, r.resolve("a"))
    // `b` was computed while `a` was in progress, so its result depends on where the cycle was
    // entered. Entries are only valid for the operation that made them: `GlbLubs` clears its
    // tables when the outermost `lub` or `glb` finishes.
    assertEquals(ErrorType, r.guard.results("b"))
    r.guard.clear()
    assertEquals(ErrorType, r.resolve("b"))
  }

  /** A real `Any`, which is also the answer of a `lub` on a cycle, is not a guard hit. */
  @Test def memoizedAnyIsNotACycle(): Unit = {
    val guard = new MemoGuard[String]
    guard.begin("k")
    assertTrue(guard.isInProgress("k"))
    assertEquals(NothingTpe, guard.lookup("k", onCycle = NothingTpe))
    guard.end("k", AnyTpe)
    assertFalse(guard.isInProgress("k"))
    assertEquals(AnyTpe, guard.lookup("k", onCycle = NothingTpe))
    guard.clear()
    assertNull(guard.lookup("k", onCycle = NothingTpe))
  }
}
