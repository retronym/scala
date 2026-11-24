//> using options -Ycheck:_
//> using filter ^\[(Now\schecking|Not\scheckable):\s\w+\]$

// The tree checker must accept the expanded name of a trait setter mixed into a class
// (`T$_setter_$w_=`) as the setter of the field; it used to warn after every phase from fields on.

trait T { val w = 2; var v = 3 }
class C extends T
class D extends T { override val w = 4 }

object Test extends App {
  val c = new C
  c.v = 5
  println(s"${c.w} ${c.v} ${new D().w}")
}
