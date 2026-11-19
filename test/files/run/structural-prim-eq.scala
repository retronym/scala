import scala.language.reflectiveCalls

object Test {
  type EqI = Any { def ==(x: Int): Boolean }
  type NeI = Any { def !=(x: Int): Boolean }
  type EqL = Any { def ==(x: Long): Boolean }
  type EqD = Any { def ==(x: Double): Boolean }

  def eqi(t: EqI, i: Int) = t == i
  def nei(t: NeI, i: Int) = t != i
  def eql(t: EqL, l: Long) = t == l
  def eqd(t: EqD, d: Double) = t == d

  def main(args: Array[String]): Unit = {
    // 1000 is outside the Integer cache, so the two boxes are different objects
    assert(eqi(1000, 1000))
    assert(eqi(1, 1))
    assert(!eqi(1000, 1001))
    assert(!nei(1000, 1000))
    assert(nei(1000, 1001))
    assert(eqi(1000L, 1000))
    assert(eqi(1000.0, 1000))
    assert(eqi('a', 97))
    assert(eql(1000, 1000L))
    assert(eql(1L << 40, 1L << 40))
    assert(eqd(1.5, 1.5))
    assert(eqd(1000, 1000.0))
    assert(!eqd(Double.NaN, Double.NaN))
  }
}
