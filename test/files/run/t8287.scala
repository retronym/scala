class K {
  // a specialized class local to a field initialiser
  val v: Int = {
    class Sp[@specialized(Int) T](t: T) { def g(f: T => Int) = f(t) }
    new Sp(7).g(_ + 1)
  }
  // and local to a method
  def m: Int = {
    class Sp[@specialized(Int) T](t: T) { def g(f: T => Int) = f(t) }
    new Sp(8).g(_ + 1)
  }
}
object O {
  val v: Int = {
    class Sp[@specialized(Int) T](val t: T)
    new Sp(10).t
  }
}
// The constructor parameter is only read in the constructor. Without variants the field stays
// private[this], and Constructors has to keep it, because the constructor reads it from the field.
class P {
  val v = { class L[@specialized(Int) T](x0: T) { val q: T = x0 }; new L("s").q }
  def m = { class L[@specialized(Int) T](x0: T) { val q: T = x0 }; new L(12).q }
}
object Test {
  def main(a: Array[String]): Unit = {
    val k = new K
    println(k.v)
    println(k.m)
    println(O.v)
    println(new { val w = { class Sp[@specialized(Int) T](val t: T); new Sp(11).t } }.w)
    val p = new P
    println(p.v)
    println(p.m)
  }
}
