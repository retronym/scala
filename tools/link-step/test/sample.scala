package t

trait T { def foo = 1; val x = 2; var y = 3; object O; lazy val lz = 4; private val p = 5; def usep = p }
trait S extends T { override def foo = super.foo + 1 }
abstract class B[A] { def get(a: A): A }
class C extends B[Int] with S { def get(a: Int) = a }
class D(i: Int) extends C
object Obj extends T
trait U extends (Int => Int) { def apply(i: Int) = i }
class E extends U
class F extends Comparable[F] { def compareTo(o: F) = 0 }
class Outer { class Inner extends T; object InnerObj extends S; def anon = new T {} }
class G extends AnyRef with T { override lazy val lz = 9 }
class H extends Function1[Int, Int] { def apply(i: Int) = i }
class V(val i: Int) extends AnyVal
class Comp extends T
object Comp extends S
abstract class Params(a: Int, b: => String, c: String*)(implicit d: Ordering[Int])
class P2 extends Params(1, "", "a")(Ordering.Int) with T
trait L { lazy val l1 = 1; lazy val l2 = "" }
class K extends L { lazy val own1 = 2; lazy val own2 = 3; def use = own1 + own2 }
class K2 extends L { lazy val own1 = 2; lazy val own2 = 3; lazy val own3 = 4; lazy val own4 = 5; lazy val own5 = 6; lazy val own6 = 7; lazy val own7 = 8; lazy val own8 = 9 }
