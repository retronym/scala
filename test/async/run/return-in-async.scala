//> using options -Xasync

import scala.concurrent._
import scala.concurrent.duration._
import ExecutionContext.Implicits.global
import scala.tools.testkit.async.Async.{async, await}

object Test {
  def f[T](t: T): Future[T] = Future.successful(t)
  // A return from a method or class nested in the block is an ordinary return.
  def nestedMethod(): Future[Int] = async {
    def g(x: Int): Int = { if (x > 0) return 1; List(x).foreach(y => return y - 1); 2 }
    g(await(f(1))) + g(0)
  }
  def nestedClass(): Future[Int] = async {
    class C { def g(x: Int): Int = { List(x).foreach(y => if (y > 0) return y); 2 } }
    val c = new C()
    c.g(await(f(1))) + c.g(0)
  }
  def main(args: Array[String]): Unit = {
    println(Await.result(nestedMethod(), 5.seconds))
    println(Await.result(nestedClass(), 5.seconds))
  }
}
