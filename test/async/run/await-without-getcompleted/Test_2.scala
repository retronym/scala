//> using options -Xasync

import scala.concurrent._
import scala.concurrent.duration.Duration
import ExecutionContext.Implicits.global
import nogetcompleted.Async.{async, await}

class Service(val name: String) {
  def call(i: Int): Future[Int] = Future.successful(i + name.length)
}

object Test {
  // The awaited expression is the last use of `s`, a field of the state machine that is set to null on leaving the state.
  def lastUseInAwaitable(s0: Service): Future[Int] = async {
    val s = s0
    val x = await(Future.successful(1))
    val y = await(s.call(x))
    val z = await(Future.successful(y))
    z
  }
  def lastUseInAwaitableInBranch(s0: Service, b: Boolean): Future[Int] = async {
    val s = s0
    val x = await(Future.successful(1))
    val y = if (b) await(s.call(x)) else 0
    val z = await(Future.successful(y))
    z
  }
  def main(args: Array[String]): Unit = {
    assert(Await.result(lastUseInAwaitable(new Service("ab")), Duration.Inf) == 3)
    assert(Await.result(lastUseInAwaitableInBranch(new Service("ab"), true), Duration.Inf) == 3)
    assert(Await.result(lastUseInAwaitableInBranch(new Service("ab"), false), Duration.Inf) == 0)
  }
}
