// The clash with an inherited member does not depend on the order of the declarations in the class:
// `f(List[String])` would override `A.f(List[Int])` in the JVM.

class A { def f(x: List[Int]): String = "A.f(List[Int])" }

class B1 extends A {
  def f(x: List[String]): String = "B1.f(List[String])"
  def f(x: String): String = "B1.f(String)"
}

class B2 extends A {
  def f(x: String): String = "B2.f(String)"
  def f(x: List[String]): String = "B2.f(List[String])"
}

class B3 extends A {
  def f(x: String): String = "B3.f(String)"
  def f(x: Int): String = "B3.f(Int)"
  def f(x: List[String]): String = "B3.f(List[String])"
}

trait T1 { def f(x: String): String = "T1.f(String)" }

// the member that clashes is in a trait
trait U1 extends A {
  def f(x: String): String = "U1.f(String)"
  def f(x: List[String]): String = "U1.f(List[String])"
}

// okay
class E extends A {
  def f(x: String): String = "E.f(String)"
  def f(x: List[String], y: Int): String = "E.f(List[String], Int)"
  override def f(x: List[Int]): String = "E.f(List[Int])"
}
class F extends E with T1 {
  override def f(x: String): String = "F.f(String)"
}
