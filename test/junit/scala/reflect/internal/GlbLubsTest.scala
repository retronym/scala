package scala.reflect.internal

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.tools.nsc.symtab.SymbolTableForUnitTesting

@RunWith(classOf[JUnit4])
class GlbLubsTest {
  object symbolTable extends SymbolTableForUnitTesting
  import symbolTable._, definitions._

  /** Classes `B` and `C` extending `P`, all with a member `f`. Lubbing `B` and `C` forces the
   *  members' infos and checks they conform to `P.f`.
   */
  private def classes(pf: Type, bf: Type => Type, cf: Type => Type): List[Type] = {
    val owner = rootMirror.RootClass
    def cls(name: String, parent: Type, fInfo: Type): Symbol = {
      val c = owner.newClassSymbol(TypeName(name), NoPosition, 0L)
      val f = c.newMethod(TermName("f"), NoPosition, 0L)
      c.setInfo(ClassInfoType(List(parent), newScopeWith(f), c))
      f.setInfo(fInfo)
      c
    }
    val p = cls("P", AnyRefTpe, NullaryMethodType(pf))
    List(cls("B", p.tpe, bf(p.tpe)), cls("C", p.tpe, cf(p.tpe))).map(_.tpe)
  }

  /** A top-level lub nested in another lub, here run by a completer, must keep the outer lub's
   *  recursion guard. The outer lub can be top-level or have an explicit depth, as when called
   *  from `solve` or `mergePrefixAndArgs`.
   */
  @Test def nestedLubKeepsOuterGuard(): Unit = {
    def check(outer: List[Type] => Type, outerDepth: List[Type] => Depth): Unit = {
      val outerFrame = scratch.frame
      var ts: List[Type] = Nil
      var guardKept: Option[Boolean] = None
      val completer = new LazyType {
        override def complete(sym: Symbol): Unit = {
          lub(List(IntTpe, StringTpe))
          assertTrue("a completer has its own tables", glbLubs.lubResults.isEmpty)
          guardKept = Some(outerFrame.lubs.isInProgress((outerDepth(ts), ts)))
          sym.setInfo(NullaryMethodType(AnyTpe))
        }
      }
      ts = classes(AnyTpe, _ => completer, NullaryMethodType(_))
      outer(ts)
      assertEquals(Some(true), guardKept)
      assertTrue(glbLubs.lubResults.isEmpty)
    }
    check(lub(_), lubDepth)
    check(lub(_, Depth(3)), _ => Depth(3))
  }

  /** A top-level lub that re-enters itself through something other than a completer, which
   *  would detect the cycle, must still terminate.
   */
  @Test def selfReentrantLubTerminates(): Unit = {
    val annotated = AnnotatedType(List(AnnotationInfo(TypeConstraintClass.tpe, Nil, Nil)), IntTpe)
    val ts = classes(annotated, _ => NullaryMethodType(IntTpe), _ => NullaryMethodType(IntTpe))
    val limit = 20
    var nesting, maxNesting = 0
    addAnnotationChecker(new AnnotationChecker {
      def annotationsConform(tpe1: Type, tpe2: Type): Boolean = {
        if (nesting < limit) {
          nesting += 1
          maxNesting = maxNesting max nesting
          try lub(ts) finally nesting -= 1
        }
        true
      }
    })
    try lub(ts)
    finally removeAllAnnotationCheckers()
    assertTrue(s"lub re-entered itself $maxNesting times", maxNesting < limit)
  }

  /** A completion must see empty guards, and they must be restored afterwards. */
  @Test def completionHasItsOwnFrame(): Unit = {
    val outer = scratch.frame
    var inner: Frame = null
    val sym = rootMirror.RootClass.newMethod(TermName("completed"), NoPosition, 0L)
    outer.subTypes.enter()
    try {
      sym.setInfo(new LazyType {
        override def complete(s: Symbol): Unit = {
          inner = scratch.frame
          assertEquals(0, subsametypeRecursions)
          s.setInfo(NullaryMethodType(IntTpe))
        }
      })
      sym.info
    } finally outer.subTypes.exit()
    assertNotSame(outer, inner)
    assertSame(outer, scratch.frame)
    assertSame("frames are reused", inner, outer.next)
  }

  /** A real `Any` is not a guard hit. */
  @Test def memoizedAnyIsNotACycle(): Unit = {
    val frame = scratch.frame
    val key = (Depth(1), List(IntTpe))
    frame.lubs.begin(key)
    assertTrue(frame.lubs.isInProgress(key))
    assertEquals(NothingTpe, frame.lubs.lookup(key, onCycle = NothingTpe))
    frame.lubs.end(key, AnyTpe)
    assertFalse(frame.lubs.isInProgress(key))
    assertEquals(AnyTpe, frame.lubs.lookup(key, onCycle = NothingTpe))
    frame.lubs.results.clear()
    assertNull(frame.lubs.lookup(key, onCycle = NothingTpe))
  }

  @Test def recursionGuardActivatesAtThreshold(): Unit = {
    val frame = scratch.frame
    val guard = new frame.RecursionGuard[String](2)
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
    val frame = scratch.frame
    val guard = new frame.SubTypeGuard(3)
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
