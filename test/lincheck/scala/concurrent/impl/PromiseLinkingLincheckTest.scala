package scala.concurrent.impl

import org.jetbrains.kotlinx.lincheck.annotations.{Operation, Param, Validate}
import org.jetbrains.kotlinx.lincheck.paramgen.IntGen
import org.junit.{Ignore, Test}

import LincheckSupport._

/** `linkRootOf` (the promise compression behind `flatMap`/`transformWith`/`recoverWith`) racing with completion
 *  and with other links.
 *
 *  The sequential specification is a union-find: each promise belongs to a component, which is completed as a
 *  whole. `link(i, j)` (i.e. `ps(i).linkRootOf(ps(j))`) is:
 *   - a no-op if `i == j` or if they are already in the same (uncompleted) component;
 *   - if both are already completed (even if they share a component): `IllegalStateException` in the strict
 *     variant, a no-op in the lenient one (see [[PromiseLinkingStrictLincheckTest]] for why);
 *   - completing the uncompleted side's component with the value of the completed side;
 *   - otherwise a union.
 *  `complete(i, v)` completes `i`'s whole component (if it isn't already); `value(i)` reads it.
 *
 *  `validate` additionally checks that no cycle of `Link`s was created (which would make `value`/`onComplete`/
 *  `tryComplete`/`toString` on any member spin forever), walking the chains without compressing them.
 *
 *  Lincheck only looks at the annotated methods of the concrete test class, hence the overrides in the subclasses.
 */
abstract class PromiseLinkingLincheckBase(strict: Boolean) {
  import PromiseLinkingLincheckBase._
  private val ps = Array.fill(N)(new Promise.DefaultPromise[Int]())

  def link(i: Int, j: Int): Unit =
    try ps(i).linkRootOf(ps(j), null)
    catch { case _: IllegalStateException if !strict => }
  def complete(i: Int, v: Int): Boolean = ps(i).trySuccess(v)
  def value(i: Int): Option[Int] = ps(i).value.map(_.get)

  def validate(): Unit = {
    // don't use `DefaultPromise.toString` in the message: it spins on a cycle
    def states = ps.zipWithIndex.map { case (p, i) =>
      p.get() match {
        case l: Promise.Link[_] => s"$i -> ${ps.indexWhere(_ eq l.get())}"
        case s                  => s"$i: ${s.getClass.getSimpleName}"
      }
    }.mkString(", ")
    for (i <- 0 until N) {
      var p = ps(i)
      var steps = 0
      while (p.get().isInstanceOf[Promise.Link[_]]) {
        steps += 1
        assert(steps <= N, s"cycle of links from promise $i: $states")
        p = p.get().asInstanceOf[Promise.Link[Int]].get()
      }
    }
    ps.foreach(_.toString)
  }
}

object PromiseLinkingLincheckBase {
  final val N = 4

  class Spec {
    protected def strict = true
    private val parent = Array.tabulate(N)(identity)
    private val values = Array.fill[Option[Int]](N)(None) // indexed by root
    private def find(i: Int): Int = if (parent(i) == i) i else find(parent(i))

    def link(i: Int, j: Int): Unit = if (i != j) {
      val ri = find(i)
      val rj = find(j)
      (values(ri), values(rj)) match {
        case (Some(_), Some(_)) => if (strict) throw new IllegalStateException("Cannot link completed promises together")
        case _ if ri == rj      =>
        case (Some(v), None)    => values(rj) = Some(v)
        case (None, Some(v))    => values(ri) = Some(v)
        case (None, None)       => parent(ri) = rj
      }
    }
    def complete(i: Int, v: Int): Boolean = { val r = find(i); values(r).isEmpty && { values(r) = Some(v); true } }
    def value(i: Int): Option[Int] = values(find(i))
  }
  class LenientSpec extends Spec { override protected def strict = false }
}

/** All operations in parallel (lenient about `IllegalStateException`).
 *
 *  FAILS (livelock): `link(i, j) || link(j, i)` on two uncompleted roots can create a `Link` cycle. Each side
 *  compresses its new `Link` to the other promise while that is still in a `Callbacks` state, then CASes its own state
 *  from `Callbacks` to the `Link`; both CASes succeed as they are on different promises. Afterwards
 *  `value`/`isCompleted`/`onComplete`/`tryComplete`/`toString` on any member of the cycle spin forever in
 *  `Link.compressed`. Sequentially (and in the union-find spec) the second link is a no-op, as both are already
 *  in the same component. Minimal scenario found by the stress strategy (thread dump: `toString` spinning in
 *  `Link.compressed`): `[] || link(2, 0) | link(0, 2) || []`.
 *  Reachable from user code via e.g. two `flatMap`s whose functions return each other's result futures, with
 *  the inputs completing concurrently.
 */
class PromiseLinkingLincheckTest extends PromiseLinkingLincheckBase(strict = false) {
  @Operation override def link(@Param(gen = classOf[IntGen], conf = "0:3") i: Int, @Param(gen = classOf[IntGen], conf = "0:3") j: Int): Unit = super.link(i, j)
  @Operation override def complete(@Param(gen = classOf[IntGen], conf = "0:3") i: Int, @Param(gen = classOf[IntGen], conf = "1:2") v: Int): Boolean = super.complete(i, v)
  @Operation override def value(@Param(gen = classOf[IntGen], conf = "0:3") i: Int): Option[Int] = super.value(i)
  @Validate override def validate(): Unit = super.validate()

  @Ignore("finds a Link cycle (livelock), see the class comment")
  @Test def stress(): Unit = check(LincheckSupport.stress().sequentialSpecification(classOf[PromiseLinkingLincheckBase.LenientSpec]), getClass)
  @Ignore("finds a Link cycle (livelock), see the class comment")
  @Test def modelChecking(): Unit = check(LincheckSupport.modelChecking(iterations = 100, invocations = 1000).sequentialSpecification(classOf[PromiseLinkingLincheckBase.LenientSpec]), getClass)
}

/** Links on one thread only (`nonParallelGroup`), racing with completions and reads; strict about
 *  `IllegalStateException`.
 *
 *  FAILS (benign): the "Cannot link completed promises together" check is racy. With `ps(2)` completed with `1`:
 *  `link(0, 2) || complete(0, 2)`: `linkRootOf` reads `ps(0)` as uncompleted, compresses the new `Link` to `ps(2)`,
 *  finds it completed and calls `ps(0).unlink(Success(1))`, which loses against `complete(0, 2)` and returns
 *  quietly. So `link` returns normally although both sides were completed with different values; no linearization
 *  explains that (the spec throws, or `complete` returns false). Found by model checking.
 */
class PromiseLinkingStrictLincheckTest extends PromiseLinkingLincheckBase(strict = true) {
  @Operation(nonParallelGroup = "link")
  override def link(@Param(gen = classOf[IntGen], conf = "0:3") i: Int, @Param(gen = classOf[IntGen], conf = "0:3") j: Int): Unit = super.link(i, j)
  @Operation override def complete(@Param(gen = classOf[IntGen], conf = "0:3") i: Int, @Param(gen = classOf[IntGen], conf = "1:2") v: Int): Boolean = super.complete(i, v)
  @Operation override def value(@Param(gen = classOf[IntGen], conf = "0:3") i: Int): Option[Int] = super.value(i)
  @Validate override def validate(): Unit = super.validate()

  @Ignore("the IllegalStateException check in linkRootOf is racy, see the class comment")
  @Test def modelChecking(): Unit = check(LincheckSupport.modelChecking(iterations = 100, invocations = 1000).sequentialSpecification(classOf[PromiseLinkingLincheckBase.Spec]), getClass)
}

/** Links on one thread only (`nonParallelGroup`), racing with completions and reads; lenient about
 *  `IllegalStateException`. This is the variant that is expected to pass. */
class PromiseLinkingSequentialLinksLincheckTest extends PromiseLinkingLincheckBase(strict = false) {
  @Operation(nonParallelGroup = "link")
  override def link(@Param(gen = classOf[IntGen], conf = "0:3") i: Int, @Param(gen = classOf[IntGen], conf = "0:3") j: Int): Unit = super.link(i, j)
  @Operation override def complete(@Param(gen = classOf[IntGen], conf = "0:3") i: Int, @Param(gen = classOf[IntGen], conf = "1:2") v: Int): Boolean = super.complete(i, v)
  @Operation override def value(@Param(gen = classOf[IntGen], conf = "0:3") i: Int): Option[Int] = super.value(i)
  @Validate override def validate(): Unit = super.validate()

  @Test def stress(): Unit = check(LincheckSupport.stress().sequentialSpecification(classOf[PromiseLinkingLincheckBase.LenientSpec]), getClass)
  @Test def modelChecking(): Unit = check(LincheckSupport.modelChecking().sequentialSpecification(classOf[PromiseLinkingLincheckBase.LenientSpec]), getClass)
}
