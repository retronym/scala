import java.util.concurrent.{Executors, ThreadFactory}

import scala.concurrent._, duration._

// Which idioms deadlock when the initializer of an object hands work to another thread and waits for it
// (scala/bug#9076). The other thread blocks until the object's class is initialized if it needs that class: to call
// the lambda body, which is a static method of the module class, or to read MODULE$. The initializing thread holds
// the initialization lock while it waits. The check file records the outcome of each idiom.
object H {
  private val daemons: ThreadFactory = r => { val t = new Thread(r); t.setDaemon(true); t }
  // Each call gets its own thread, since the thread of an idiom that hangs stays blocked.
  def await[A](f: ExecutionContext => Future[A]): String = {
    val ec = ExecutionContext.fromExecutorService(Executors.newSingleThreadExecutor(daemons))
    try { Await.result(f(ec), 2.seconds); "ok" } catch { case _: TimeoutException => "HANG" }
  }
  def thread(r: Runnable): String = { val t = daemons.newThread(r); t.start(); t.join(2000); if (t.isAlive) "HANG" else "ok" }
}

// the lambda does not mention the object
object FutureConstant { val r = H.await(Future(42)(_)) }
// the lambda calls a method of the object
object FutureCallsMember { def f = 42; val r = H.await(Future(f)(_)) }
// a thread started in the initializer, the lambda does not mention the object
object ThreadLambda { val r = H.thread(() => ()) }
// the same with an anonymous class
object ThreadAnonClass { val r = H.thread(new Runnable { def run(): Unit = () }) }
// an anonymous class that reads a val of the object
object ThreadAnonClassReadsVal { val x = 1; val r = H.thread(new Runnable { def run(): Unit = x.toString }) }
// a lambda over its own parameter only
object FutureMap { val r = H.await(Future.successful(1).map(_ + 1)(_)) }
// a lambda defined in a class that the initializer instantiates
class Worker { def go(): String = H.await(Future(42)(_)) }
object LambdaInOtherClass { val r = new Worker().go() }
// the lambda calls a method of another object
object Other { def compute(): Int = 42 }
object FutureMethodOfOtherObject { val r = H.await(Future(Other.compute())(_)) }
// a lazy val is initialized after the object
object LazyValFuture { lazy val r = H.await(Future(42)(_)) }
// the body of an App runs from main, not from the static initializer
object AppBody extends App { val r = H.await(Future(42)(_)) }
// a local lazy val forced by the future
object LocalLazy { val r = { lazy val v = 42; H.await(Future(v)(_)) } }
// a by-name argument evaluated on another thread
object ByName { def later(body: => Int): String = H.thread(() => body); val r = later(42) }
// two objects that read each other, initialized from two threads
object CycleA { val x: Int = { Thread.sleep(300); CycleB.y + 1 }; val y = 1 }
object CycleB { val x: Int = { Thread.sleep(300); CycleA.y + 1 }; val y = 1 }

object Test {
  def main(args: Array[String]): Unit = {
    def show(name: String)(r: => String): Unit = println(f"$name%-28s $r")
    def force(x: Int): Unit = ()
    show("FutureConstant")(FutureConstant.r)
    show("FutureCallsMember")(FutureCallsMember.r)
    show("ThreadLambda")(ThreadLambda.r)
    show("ThreadAnonClass")(ThreadAnonClass.r)
    show("ThreadAnonClassReadsVal")(ThreadAnonClassReadsVal.r)
    show("FutureMap")(FutureMap.r)
    show("LambdaInOtherClass")(LambdaInOtherClass.r)
    show("FutureMethodOfOtherObject")(FutureMethodOfOtherObject.r)
    show("LazyValFuture")(LazyValFuture.r)
    show("AppBody")({ AppBody.main(Array()); AppBody.r })
    show("LocalLazy")(LocalLazy.r)
    show("ByName")(ByName.r)
    show("CycleOnTwoThreads") {
      val a = new Thread(() => force(CycleA.x)); val b = new Thread(() => force(CycleB.x))
      a.setDaemon(true); b.setDaemon(true); a.start(); b.start(); a.join(2000); b.join(100)
      if (a.isAlive || b.isAlive) "HANG" else "ok"
    }
  }
}
