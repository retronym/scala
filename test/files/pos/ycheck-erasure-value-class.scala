//> using options -Ycheck:erasure,posterasure
//
// Erasure represents `new VC(e)` by `e` typed as `ErasedValueType(VC, Int)`;
// the tree checker could not retype that and aborted.

class VC(val x: Int) extends AnyVal {
  def plus(y: Int): VC = new VC(x + y)
}
class VG[T](val t: T) extends AnyVal

abstract class AC[A] { def h(a: A): A }
trait T2[A] { def f: A; def g(a: A): String }

object O {
  def h(a: VC): VC = new VC(a.x + 1)
  def k(a: VC): Int = a.x + 1
  def vg[T](t: T): VG[T] = new VG(t)
  def local(): Int = { val v = new VC(1); v.plus(2).x }
  def arr: Array[VC] = Array(new VC(1))
  def opt: Option[VC] = Some(new VC(1))
  def eq(a: VC, b: VC): Boolean = a == b
}

class C6 extends AC[VC] with T2[VC] {
  def h(a: VC) = new VC(a.x + 1)
  def f = new VC(6)
  def g(a: VC) = "g6"
}
