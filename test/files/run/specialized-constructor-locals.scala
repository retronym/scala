// Constructor statements of a class with a specialized field are copied into the specialized
// subclass; locals in those statements must stay local to the constructor.

class C[@specialized(Int) A](val a: A) {
  val b: Int = Option(a) match { case Some(x) if x == a => 1; case Some(_) => 2; case None => 3 }
}
class D[@specialized(Int) A](val a: A) {
  val b: Boolean = { val t = Option(a); t.isEmpty }
}

// scala/bug#7574
class Ctor[@specialized(Int) T](var t1: T, var t2: T) {
  {
    val XXX = "XXX"
    println(XXX)
    t1 = t2
  }
}

// scala/bug#10096
class E[@specialized(Double) A]()
class F[@specialized(Double) A]() {
  val (a, b) = (1, 2)
  val c = new E[A]()
}

object Test {
  def main(args: Array[String]): Unit = {
    println(new C(1).b)
    println(new C("s").b)
    println(new D(1).b)
    println(new D("s").b)
    println(new Ctor(1, 2).t1)
    val f = new F[Double]()
    println(f.a + f.b)
  }
}
