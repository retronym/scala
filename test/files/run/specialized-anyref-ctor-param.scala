// A class whose only specialization is AnyRef has no specialized subclass, but its
// type parameter still counts as specialized, so the constructor reads `x0` from the field.
class V[@specialized(AnyRef) T](x0: T) {
  val v: T = x0
}
class W[@specialized(AnyRef) T](x0: T, xs: Array[T])(y0: T) {
  private[this] var w: T = x0
  println(y0)
  def get: T = w
  val len = xs.length
}
// no specialized variant either: Int does not conform to the bound
class X[@specialized(Int) T <: String](x0: T) {
  val v: T = x0
}
object Test {
  def main(args: Array[String]): Unit = {
    println(new V("s").v)
    println(new V(1).v)
    val w = new W("a", Array("b"))("c")
    println(w.get)
    println(w.len)
    println(new X("x").v)
    def local = {
      class L[@specialized(AnyRef) T](x0: T) { val v: T = x0 }
      new L("l").v
    }
    println(local)
  }
}
