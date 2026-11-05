package scala.reflect.internal

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.tools.nsc.symtab.SymbolTableForUnitTesting
import scala.tools.testkit.AssertUtil.assertThrows

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
    r.guard.results.clear()
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
    guard.results.clear()
    assertNull(guard.lookup("k", onCycle = NothingTpe))
  }

  /** Whether a name reaches `Int` through its edges, as `baseType` and `isVolatile` guard their
   *  recursion. On a cycle, the path being followed doesn't reach it.
   */
  class Reacher(edges: Map[String, List[String]], leaves: Map[String, Type], threshold: Int) {
    val guard = new RecursionGuard[String](threshold)

    // The canonical use of a `RecursionGuard`: `exit` in a `finally` if and only if `enter`
    // returned true, and the answer to a cycle otherwise.
    def reachesInt(name: String): Boolean =
      if (guard.enter(name))
        try leaves.get(name).exists(_ =:= IntTpe) || edges.getOrElse(name, Nil).exists(reachesInt)
        finally guard.exit(name)
      else false
  }

  @Test def recursionGuardStopsACycleOncePastTheThreshold(): Unit = {
    val edges = Map("a" -> List("b"), "b" -> List("a", "c"))
    val r = new Reacher(edges, Map("c" -> StringTpe), threshold = 4)
    assertFalse(r.reachesInt("a"))
    assertEquals(0, r.guard.depth)
    assertTrue(r.guard.pending.isEmpty)
    val r2 = new Reacher(edges, Map("c" -> IntTpe), threshold = 4)
    assertTrue("the cycle doesn't hide another path", r2.reachesInt("a"))
  }

  @Test def recursionGuardIsBalancedWhenTheBodyThrows(): Unit = {
    val r = new Reacher(Map("a" -> List("a")), Map.empty, threshold = 1) {
      override def reachesInt(name: String): Boolean =
        if (guard.enter(name)) try throw new RuntimeException("boom") finally guard.exit(name)
        else false
    }
    assertThrows[RuntimeException](r.reachesInt("a"))
    assertEquals(0, r.guard.depth)
    assertTrue(r.guard.pending.isEmpty)
  }

  @Test def recursionGuardActivatesAtThreshold(): Unit = {
    val guard = new RecursionGuard[String](2)
    assertTrue("below the threshold, keys aren't tracked", guard.enter("a"))
    assertTrue(guard.pending.isEmpty)
    assertTrue(guard.enter("a"))
    assertFalse("a cycle", guard.enter("a"))
    assertEquals("a cycle leaves the depth as it was", 2, guard.depth)
    guard.exit("a")
    guard.exit("a")
    assertEquals(0, guard.depth)
    assertTrue(guard.pending.isEmpty)
  }

  @Test def subTypeGuardRemembersCyclesForTheOutermostCheck(): Unit = {
    val guard = new SubTypeGuard(3, new UndoLog)
    guard.enter()
    assertTrue(guard.enter(IntTpe, StringTpe))
    assertTrue(guard.enter(IntTpe, StringTpe))
    assertFalse("a cycle", guard.enter(IntTpe, StringTpe))
    assertEquals("a cycle leaves the depth as it was", 3, guard.depth)
    guard.exit(IntTpe, StringTpe)
    guard.exit(IntTpe, StringTpe)
    assertFalse("known to be false, below the threshold too", guard.enter(IntTpe, StringTpe))
    assertEquals(1, guard.depth)
    guard.exit()
    assertEquals(0, guard.depth)
    assertTrue(guard.pending.isEmpty)
    assertTrue("forgotten after the outermost check", guard.knownFalse.isEmpty)
    assertTrue(guard.enter(IntTpe, StringTpe))
    guard.exit(IntTpe, StringTpe)
  }
}
