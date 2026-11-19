package scala.util.control

import org.junit.Assert.assertEquals
import org.junit.Test

import scala.annotation.tailrec
import scala.util.control.TailCalls._

class TailCallsTest {
  private final val N = 1000000

  private def inc(x: Int): TailRec[Int] = done(x + 1)

  @tailrec private def stepAll[A](t: TailRec[A]): A = t.resume match {
    case Right(a) => a
    case Left(k)  => stepAll(k())
  }

  private def leftNested(n: Int)(bind: TailRec[Int] => TailRec[Int]): TailRec[Int] =
    (1 to n).foldLeft(tailcall(done(0)))((acc, _) => bind(acc))

  private def rightNested(i: Int): TailRec[Int] =
    if (i == N) done(0) else tailcall(done(1)).flatMap(x => rightNested(i + 1).map(_ + x))

  // Each continuation returns a left-nested chain, and the results are themselves bound left-nested.
  private def mixed(width: Int): TailRec[Int] = {
    def chain(x: Int): TailRec[Int] = (1 to width).foldLeft(tailcall(done(x)))((acc, _) => acc.flatMap(inc))
    def loop(i: Int): TailRec[Int] =
      if (i == width) done(0) else chain(0).flatMap(x => loop(i + 1).flatMap(y => chain(y).map(_ + x - width)))
    (1 to width).foldLeft(loop(0))((acc, _) => acc.flatMap(chain).map(_ - width))
  }

  private def fib(n: Int): TailRec[Int] =
    if (n < 2) done(n) else for {
      x <- tailcall(fib(n - 1))
      y <- tailcall(fib(n - 2))
    } yield x + y

  @Test def leftNestedFlatMap(): Unit =
    assertEquals(N, leftNested(N)(_.flatMap(inc)).result)

  @Test def leftNestedFlatMapOnDone(): Unit =
    assertEquals(N, (1 to N).foldLeft(done(0))((acc, _) => acc.flatMap(inc)).result)

  @Test def leftNestedMap(): Unit =
    assertEquals(N, leftNested(N)(_.map(_ + 1)).result)

  @Test def rightNestedFlatMap(): Unit =
    assertEquals(N, rightNested(0).result)

  @Test def rightNestedTailPosition(): Unit = {
    def loop(i: Int, acc: Int): TailRec[Int] =
      if (i == N) done(acc) else tailcall(done(1)).flatMap(x => loop(i + 1, acc + x))
    assertEquals(N, loop(0, 0).result)
  }

  @Test def mixedNesting(): Unit =
    assertEquals(1000 * 1000, mixed(1000).result)

  @Test def bindsRunInOrder(): Unit = {
    val t = (1 to 5).foldLeft(tailcall(done(List.empty[Int])))((acc, i) => acc.flatMap(xs => done(i :: xs)).map(identity))
    assertEquals(List(5, 4, 3, 2, 1), t.result)
    assertEquals(List(5, 4, 3, 2, 1), stepAll(t))
  }

  @Test def resumeLeftNested(): Unit = {
    assertEquals(N, stepAll(leftNested(N)(_.flatMap(inc))))
    assertEquals(N, stepAll(leftNested(N)(_.map(_ + 1))))
  }

  @Test def resumeRightNested(): Unit =
    assertEquals(N, stepAll(rightNested(0)))

  @Test def resumeMixedNesting(): Unit =
    assertEquals(1000 * 1000, stepAll(mixed(1000)))

  @Test def fibonacci(): Unit = {
    assertEquals(6765, fib(20).result)
    assertEquals(6765, stepAll(fib(20)))
  }
}
