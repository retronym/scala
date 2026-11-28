// A candidate whose expansion diverges fails; it does not fail the candidates tried after it.
// Each object holds the same definitions in a different order.
class T(val n: String)
class T1(n: String) extends T(n)
object First {
  implicit val a: T1 = new T1("a")
  implicit def b(implicit t: T): T = new T("b(" + t.n + ")")
  implicit def c(implicit t: T): T = new T("c(" + t.n + ")")
}
object Second {
  implicit def b(implicit t: T): T = new T("b(" + t.n + ")")
  implicit val a: T1 = new T1("a")
  implicit def c(implicit t: T): T = new T("c(" + t.n + ")")
}
object Last {
  implicit def c(implicit t: T): T = new T("c(" + t.n + ")")
  implicit def b(implicit t: T): T = new T("b(" + t.n + ")")
  implicit val a: T1 = new T1("a")
}

object Fwd {
  class K0(val n: String)
  class K1(n: String) extends K0(n)
  class K2(val n: String)
  implicit def d0(implicit p0: K1): K2 = new K2("d0(" + p0.n + ")")
  implicit val v1: K0 = new K0("v1")
  implicit def d2(implicit p0: K2): K1 = new K1("d2(" + p0.n + ")")
  implicit val v3: K2 = new K2("v3")
}
object Rev {
  class K0(val n: String)
  class K1(n: String) extends K0(n)
  class K2(val n: String)
  implicit val v3: K2 = new K2("v3")
  implicit def d2(implicit p0: K2): K1 = new K1("d2(" + p0.n + ")")
  implicit val v1: K0 = new K0("v1")
  implicit def d0(implicit p0: K1): K2 = new K2("d0(" + p0.n + ")")
}

// Two recursive rules and a base case, imported, with a fallback in the companion.
package p {
  class TC[A](val id: String)
  object TC {
    implicit def fallback[A]: TC[List[List[A]]] = new TC("fallback in companion")
  }
  object RulesFirst {
    implicit def x[A](implicit a: TC[List[A]]): TC[A] = new TC("x(" + a.id + ")")
    implicit def y[A](implicit a: TC[List[List[A]]]): TC[List[A]] = new TC("y(" + a.id + ")")
    implicit def good[A]: TC[List[List[A]]] = new TC("good")
  }
  object RulesLast {
    implicit def good[A]: TC[List[List[A]]] = new TC("good")
    implicit def x[A](implicit a: TC[List[A]]): TC[A] = new TC("x(" + a.id + ")")
    implicit def y[A](implicit a: TC[List[List[A]]]): TC[List[A]] = new TC("y(" + a.id + ")")
  }
}

// scala/bug#13158
trait Foo[A]
object Foo {
  implicit def good[A]: Foo[A] = new Foo[A] {}
  implicit def div1[A](implicit f: Foo[A]): Foo[A] = f
  implicit def div2[A](implicit f: Foo[A]): Foo[A] = f
}

object Test {
  def foo[X](implicit o: Foo[X]): String = "foo"

  def main(args: Array[String]): Unit = {
    println({ import First._; implicitly[T].n })
    println({ import Second._; implicitly[T].n })
    println({ import Last._; implicitly[T].n })
    println({ import Fwd._; implicitly[K0].n })
    println({ import Rev._; implicitly[K0].n })
    println({ import p._, RulesFirst._; implicitly[TC[Int]].id })
    println({ import p._, RulesLast._; implicitly[TC[Int]].id })
    println(foo)
  }
}
