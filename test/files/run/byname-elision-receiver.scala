//> using options -nowarn

// This test fails: it records the expected behaviour of a by-name argument `q.f()` with
// `f: () => Int`. UnCurry passes `q.f` instead of a new thunk (scala/bug#1247), which
// evaluates `q.f` at the call site rather than when the argument is evaluated.
class A { val f: () => Int = () => 1 }

class B(store: (=> Int) => Unit) {
  store(f()) // `f` is not initialized yet
  val f: () => Int = () => 2
}

object Test {
  def never(x: => Int): Unit = ()

  def main(args: Array[String]): Unit = {
    val a: A = null
    try { never(a.f()); println("not evaluated") }
    catch { case _: NullPointerException => println("NPE at the call site") }

    var thunk: () => Int = null
    new B(x => thunk = () => x)
    println(try thunk() catch { case _: NullPointerException => "NPE: captured the uninitialized field" })
  }
}
