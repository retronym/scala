// An `abstract override` in a trait over a specialized abstract class: the super call must reach the
// implementation in the class the trait is mixed into, not the trait's own override again.
abstract class B5[@specialized(Int) T] { def m(t: T): T }
class C5 extends B5[Int] { def m(t: Int) = t }
trait M5 extends B5[Int] { abstract override def m(t: Int) = super.m(t) + 1 }

abstract class BR[@specialized(Int) T] { def m(t: T): T }
class CR extends BR[String] { def m(t: String) = t }
trait MR extends BR[String] { abstract override def m(t: String) = super.m(t) + "!" }

// The implementation in a generic subclass, which has no specialized override
class CG[T] extends B5[T] { def m(t: T) = t }

// Stacked: the super call of M5 must reach the specialized override in M6
trait M6 extends B5[Int] { abstract override def m(t: Int) = super.m(t) * 10 }

// A plain override of a concrete specialized method, super call through the accessor in the trait
class B7[@specialized(Int) T] { def m(t: T): T = t }
trait M7 extends B7[Int] { override def m(t: Int) = super.m(t) + 1 }

// The same, into a specialized trait
trait TB[@specialized(Int) T] { def m(t: T): T = t }
trait MT extends TB[Int] { override def m(t: Int) = super.m(t) + 1 }

object Test {
  def main(args: Array[String]): Unit = {
    val c = new C5 with M5
    println(c.m(1))
    println((c: B5[Int]).m(1))
    println((new CG[Int] with M5).m(1))
    println((new C5 with M6 with M5).m(1))
    println((new C5 with M5 with M6).m(1))
    println((new B7[Int] with M7).m(1))
    class C7 extends B7[Int] { override def m(t: Int) = t * 3 }
    println((new C7 with M7).m(1))
    println((new TB[Int] with MT).m(1))
    val r = new CR with MR
    println(r.m("a"))
    println((r: BR[String]).m("a"))
  }
}
