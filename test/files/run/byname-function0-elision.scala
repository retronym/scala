// A by-name argument `f()` is passed as the Function0 `f` itself only if
// evaluating `f` at the call site cannot be observed.
object Fn extends (() => Int) {
  println("Fn initialized")
  def apply() = 1
}

object Holder {
  println("Holder initialized")
  val f: () => Int = () => 2
}

class C {
  lazy val member: () => Int = { println("member forced"); () => 3 }
  val strict: () => Int = () => 4
}

// a val implemented by a lazy val: nothing in the selection `t.v` says that it has an effect
trait T { val v: () => Int }
class L extends T { lazy val v: () => Int = { println("L.v forced"); () => 8 } }

class Early(store: (=> Int) => Unit) {
  store(f()) // `f` is not initialized yet
  val f: () => Int = () => 9
}

class Fun extends (() => Int) {
  def apply() = 10
  def self(same: (=> Int) => Boolean): Boolean = same(this())
}

object Test {
  def never(x: => Int): Unit = ()
  def once(x: => Int): Int = x

  lazy val lz: () => Int = { println("lz forced"); () => 5 }

  def main(args: Array[String]): Unit = {
    lazy val local: () => Int = { println("local forced"); () => 6 }
    val c = new C

    println("not evaluated:")
    never(lz())
    never(local())
    never(c.member())
    never(Fn())
    never(Holder.f())
    val t: T = new L
    never(t.v())
    val nullC: C = null
    never(nullC.strict())

    println("evaluated:")
    println(once(lz()))
    println(once(local()))
    println(once(c.member()))
    println(once(Fn()))
    println(once(Holder.f()))
    println(once(t.v()))
    println(once(c.strict()))
    var thunk: () => Int = null
    new Early(x => thunk = () => x)
    println(thunk())

    // a local value and `this` are still passed directly
    val f: () => Int = () => 7
    println(once(f()))
    def isSame(x: => Int)(g: () => Int) = { val thunk = () => x; thunk eq g }
    def isSameAs(g: () => Int)(x: => Int) = isSame(x)(g)
    println(isSameAs(f)(f()))
    val fun = new Fun
    println(fun.self(isSameAs(fun)(_)))
    println(isSameAs(c.strict)(c.strict()))
  }
}
