//> using options -Xasync

import scala.concurrent._, duration._
import scala.tools.testkit.async.Async.{async, await}
import ExecutionContext.Implicits.global

// A val in a loop body is a fresh binding per iteration; a function created in that iteration
// keeps that iteration's value even when the val is lifted to a field of the state machine.
object Test {
  def force(thunk: => Int): () => Int = () => thunk

  def main(args: Array[String]): Unit = {
    // function created before the await
    val r1 = Await.result(async {
      var i = 0; var fns = List.empty[() => Int]
      while (i < 3) {
        val k = i * 10
        fns ::= (() => k)
        await(Future.successful(()))
        i += k * 0 + 1
      }
      fns.map(_())
    }, 10.seconds)
    println(r1)

    // function created after the await, by-name argument, nested function
    val r2 = Await.result(async {
      var i = 0; var fns = List.empty[() => Int]
      while (i < 3) {
        val k = i * 10
        val s = await(Future.successful("k=" + k))
        fns ::= (() => k + s.length)
        fns ::= force(k)
        fns ::= (() => List(1).map(_ + k).head)
        i += 1
      }
      fns.map(_())
    }, 10.seconds)
    println(r2)

    // a function sees a later assignment to a var that it captures
    val r3 = Await.result(async {
      var i = 0; var res = List.empty[Int]
      while (i < 3) {
        var v = i
        val f = () => v
        v += await(Future.successful(100))
        res ::= f()
        i += 1
      }
      res
    }, 10.seconds)
    println(r3)
  }
}
