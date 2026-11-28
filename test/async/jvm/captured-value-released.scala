//> using options -Xasync

import java.lang.ref.WeakReference
import scala.concurrent._
import scala.tools.testkit.async.Async.{async, await}
import ExecutionContext.Implicits.global

// An async block uses a value for the last time before an await and then waits on a future that never completes.
// The state machine is reachable from that future. Is the value?
object Test {
  final class Big { val bytes = new Array[Byte](1 << 16); def use(): Int = bytes.length }

  def done[T](t: T): Future[T] = Future.successful(t)

  // a parameter of the enclosing method
  def enclosingParam(big: Big, never: Future[Int]): Future[Int] = async {
    val n = big.use()
    await(never) + n
  }
  def enclosingParamTwoAwaits(big: Big, never: Future[Int]): Future[Int] = async {
    val n = await(done(big.use()))
    val m = await(done(n + 1))
    await(never) + m
  }
  // a local value of the enclosing method
  def enclosingVal(holder: Array[Big], never: Future[Int]): Future[Int] = {
    val big = holder(0)
    holder(0) = null
    async {
      val n = big.use()
      await(never) + n
    }
  }
  // a local value of the block
  def blockLocal(holder: Array[Big], never: Future[Int]): Future[Int] = async {
    val big = holder(0)
    holder(0) = null
    val x = await(done(1))
    val n = big.use() + x
    await(never) + n
  }
  // the same steps written with flatMap
  def flatMapChain(big: Big, never: Future[Int]): Future[Int] =
    Future(big.use()).flatMap(n => never.map(_ + n))

  def start(body: (Big, Future[Int]) => Future[Int]): (AnyRef, WeakReference[Big]) = {
    val big = new Big
    val never = Promise[Int]()
    val result = body(big, never.future)
    ((never, result), new WeakReference(big))
  }

  def check(name: String)(body: (Big, Future[Int]) => Future[Int]): Unit = {
    val (parked, ref) = start(body)
    var i = 0
    while (i < 50 && ref.get != null) {
      System.gc()
      Thread.sleep(20)
      i += 1
    }
    println(s"$name: ${if (ref.get == null) "released" else "retained"}")
    assert(parked != null)
  }

  def main(args: Array[String]): Unit = {
    check("local of the async block")((big, never) => blockLocal(Array(big), never))
    check("flatMap chain")(flatMapChain)
    check("parameter of the enclosing method")(enclosingParam)
    check("parameter of the enclosing method, two awaits")(enclosingParamTwoAwaits)
    check("value of the enclosing method")((big, never) => enclosingVal(Array(big), never))
  }
}
