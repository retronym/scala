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

    println("evaluated:")
    println(once(lz()))
    println(once(local()))
    println(once(c.member()))
    println(once(Fn()))
    println(once(Holder.f()))

    // still passed directly
    val f: () => Int = () => 7
    println(once(f()))
    println(once(c.strict()))
    def isSame(x: => Int)(g: () => Int) = { val thunk = () => x; thunk eq g }
    def isSameAs(g: () => Int)(x: => Int) = isSame(x)(g)
    println(isSameAs(f)(f()))
    println(isSameAs(c.strict)(c.strict()))
  }
}
