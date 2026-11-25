//> using options -Xasync

import scala.concurrent._, duration._
import scala.tools.testkit.async.Async.{async, await}
import ExecutionContext.Implicits.global

// Elements of a repeated argument to the left of an awaiting element are evaluated before it.
object Test {
  var log = List.empty[String]
  def e[T](s: String, t: T): T = { log ::= s; t }

  def main(args: Array[String]): Unit = {
    var x = 1
    println(Await.result(async { List(x, await(Future { x = 2; 3 })) }, 10.seconds))

    var s = "a"
    println(Await.result(async { Array[String](s, await(Future.successful { s = "b"; "c" })).toList }, 10.seconds))

    var i = 1
    println(Await.result(async { Array[Int](i, await(Future.successful { i = 2; 3 })).toList }, 10.seconds))

    var y = 1
    println(Await.result(async { java.util.Arrays.asList(y, await(Future.successful { y = 2; 3 })) }, 10.seconds))

    println(Await.result(async { List(e("a", 1), await(Future(e("b", 2))), e("c", 3), await(Future(e("d", 4))), e("e", 5)) }, 10.seconds))
    println(log.reverse)
  }
}
