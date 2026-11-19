//> using jvm 12+

import java.lang.constant.{Constable, ConstantDesc}

// Integer, Long, Float, Double and the other boxes implement `Constable` since JDK 12, the numeric
// ones `ConstantDesc` as well. Values of these static types can be boxed numbers, so `==` has to
// compare them as numbers, as it does for `Any`, `Comparable` and `Serializable`.
object Test {
  def eqAny(a: Any, b: Any) = a == b
  def eqComparable(a: Comparable[_], b: Comparable[_]) = a == b
  def eqSerializable(a: java.io.Serializable, b: java.io.Serializable) = a == b
  def eqConstable(a: Constable, b: Constable) = a == b
  def neConstable(a: Constable, b: Constable) = a != b
  def eqConstantDesc(a: ConstantDesc, b: ConstantDesc) = a == b
  def eqBounded[T <: Constable](a: T, b: T) = a == b
  def eqMixed(a: Constable, b: Any) = a == b
  def eqMixedDesc(a: Any, b: ConstantDesc) = a == b

  def main(args: Array[String]): Unit = {
    val i: Integer = 1
    val l: java.lang.Long = 1L
    val d: java.lang.Double = 1.0
    val c: Character = 'a'
    val n: Integer = 97

    assert(eqAny(i, l))
    assert(eqComparable(i, l))
    assert(eqSerializable(i, l))
    assert(i.## == l.##)

    assert(eqConstable(i, l))
    assert(!neConstable(i, l))
    assert(eqConstable(c, n))
    assert(eqConstable(n, c))
    assert(eqConstantDesc(i, l))
    assert(eqConstantDesc(l, d))
    assert(eqBounded[Constable](i, l))
    assert(eqMixed(i, l))
    assert(eqMixedDesc(i, d))

    assert(!eqConstable(i, "1"))
    assert(eqConstable("1", "1"))
    assert(eqConstable(null, null))
    assert(!eqConstable(null, i))
    assert(!eqConstantDesc(i, 2L: java.lang.Long))
  }
}
