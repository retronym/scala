trait Fn0 { def apply(): Int }
trait Get[A] { def get(a: A): Int; def get(as: List[A]): Int }
trait Box[A] { def unbox: A }

object Test {
  def sm[@specialized(Int) T](t: T): Int = (new Fn0 { def apply(): Int = 1 }).apply()

  def overloaded[@specialized(Int) T](t: T): Int =
    new Get[T] { def get(a: T): Int = 2; def get(as: List[T]): Int = 3 }.get(t)

  class C[@specialized(Int) T](t: T) {
    def m: T = new Box[T] { val v: T = t; def unbox: T = v }.unbox
  }

  def main(args: Array[String]): Unit = {
    println(sm(1))
    println(sm(""))
    println(overloaded(1))
    println(overloaded(""))
    println(new C(4).m)
    println(new C("5").m)
  }
}
