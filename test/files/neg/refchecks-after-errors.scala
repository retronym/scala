//> using options -Yrefchecks-after-errors
// refchecks diagnostics are reported even though typer reported errors,
// but not when they are merely a consequence of the typer error.
trait T { def f: Int; def g(x: Int): Int }

class Wanted extends T {
  val x: Int = "" // typer error
  def g(x: Int) = x
  override def nope = 1 // refchecks: overrides nothing
}

// no cascading errors from an unresolved parent (directly or through an ancestor)
class A extends Missing with T
class B extends A { override def run(): Unit = () }
class C extends Ordering[Undefined]

// no cascading errors from an erroneous member type
class D extends T {
  def f = undefinedName
  override def g(x: Undefined): Int = 1
}
