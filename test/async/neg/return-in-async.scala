//> using options -Xasync

import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.tools.testkit.async.Async.{async, await}
import scala.tools.partest.async.OptionAwait._

// A `return` that targets the method enclosing the async block cannot return from that method.
class ReturnInAsync {
  def f[T](t: T): Future[T] = Future.successful(t)
  def direct(cached: Boolean): Future[String] = async {
    if (await(f(cached))) return f("from cache")
    "computed"
  }
  def noAwait(): Future[Boolean] = async { return f(false) }
  def primitive(): Int = { async { if (await(f(true))) return 42; 1 }; 0 }
  def inFunction(xs: List[Int]): Future[String] = async {
    val z = await(f("z"))
    xs.foreach(x => if (x > 1) return f("function"))
    z
  }
  def inSynchronized(c: Boolean): Future[String] = async {
    val z = await(f("z"))
    this.synchronized { if (c) return f("synchronized") }
    z
  }
  def inNestedMethodsFunction(): Future[Int] = async {
    def g(x: Int): Int = { List(x).foreach(y => if (y > 0) return y); 2 }
    val h = () => return f(0)
    g(await(f(1)))
  }
  // rejected for every front end, including one that runs the block within the enclosing method
  def synchronousFrontEnd(c: Boolean): Option[String] = optionally {
    val a = value(Some("a"))
    if (c) return Some("early")
    a + value(Some("b"))
  }
  // fine: these return from a method that is nested in the block
  def nestedMethod(): Future[Int] = async {
    def g(x: Int): Int = { if (x > 0) return 1; List(x).foreach(y => return y); 2 }
    g(await(f(1)))
  }
  def nestedClass(): Future[Int] = async {
    class C { def g(x: Int): Int = { if (x > 0) return 1; List(x).foreach(y => return y); 2 } }
    new C().g(await(f(1)))
  }
}
