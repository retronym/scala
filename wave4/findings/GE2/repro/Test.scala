trait T0[@specialized(Int) B] {
  def m[X](x: X, b: B): Int = 1
}
class C extends T0[Int] {
  override def m[X](x: X, b: Int): Int = super.m[X](x, b) + 1
}
object Test { def main(a: Array[String]): Unit = println(new C().m(1, 2)) }
