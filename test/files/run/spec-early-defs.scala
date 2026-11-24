//> using options -deprecation:false
trait TV[@specialized(Int) T] { val v: T }

// The early definition is assigned before the super call in the generic constructor,
// so a specialized field for it would never be assigned in the specialized subclass.
class Early[@specialized(Int) T](x: T) extends { val early: T = x } with TV[T] { val v = x; def e = early }
class Early2[@specialized(Int) T](x: T) extends { val early: T = x } with AnyRef { def e = early }
class Early3[@specialized(Int) T](x: T) extends { val early: T = x; var ev: T = x } with AnyRef {
  val late: T = early
  def e = (early, ev, late)
  def f(y: T): T = { ev = y; early }
}
// The specialized subclass would pass null for `x`, which the early definition reads.
class Early4[@specialized(Int) T](val x: T) extends { val early: T = x } with AnyRef { def e = (early, x) }

object Test {
  def main(args: Array[String]): Unit = {
    println((new Early(3).e, new Early("s").e, new Early(3).early, (new Early(3): TV[Int]).v))
    println((new Early2(3).e, new Early2(3).early, new Early2("s").e))
    val e3 = new Early3(3)
    println(e3.e)
    e3.ev = 4
    println((e3.ev, e3.e))
    println((e3.f(5), e3.e))
    println(new Early3("s").e)
    println((new Early4(3).e, new Early4(3).early, new Early4(3).x, new Early4("s").e))
  }
}
