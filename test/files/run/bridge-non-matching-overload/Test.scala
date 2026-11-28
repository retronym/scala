// Members that the type checker treats as overloads of an inherited method, but that have the
// same parameter types after uncurry. No bridge may route a call of the inherited method to them.
class A {
  def f(xs: Seq[String]): String = "A.f(Seq[String])"
  def g(x: () => String): String = "A.g(() => String)"
  def h(x: String)(y: Int): String = "A.h(String)(Int)"
  def i(xs: String*): String = "A.i(String*)"
  def j(x: => String): String = "A.j(=> String)"
  final def k(xs: Seq[String]): String = "A.k(Seq[String])"
}
class B extends A {
  def f(xs: String*): Object = "B.f(String*)"
  def g(x: => String): Object = Integer.valueOf(1)
  def h(x: String, y: Int): Object = "B.h(String, Int)"
  def i(xs: Seq[String]): Object = "B.i(Seq[String])"
  def j(x: () => String): Object = "B.j(() => String)"
  def k(xs: String*): Object = "B.k(String*)"
}

trait K[T] {
  def f(xs: Seq[T]): String = "K.f(Seq[T])"
  def g(x: () => T): T
}
class KS extends K[String] {
  def f(xs: String*): Object = "KS.f(String*)"
  def g(x: () => String): String = "KS.g(() => String)"
}

// Uncurry rewrites the type arguments of a parent as well. Neither `f` has a by-name type of its own, and
// the two only match once G1 extends G[(() => Int) => Int]. The same through a superclass and an outer class.
class G[T] { def f(x: T): CharSequence = "G.f(T)" }
class G1 extends G[(=> Int) => Int] { def f(x: (() => Int) => Int): Object = "G1.f" }
class G2 extends G[(=> Int) => Int]
class G3 extends G2 { def f(x: (() => Int) => Int): Object = "G3.f" }
class H[T] { class I { def f(x: T): CharSequence = "H.I.f(T)" } }
object H1 extends H[(=> Int) => Int] { class I1 extends I { def f(x: (() => Int) => Int): Object = "H1.I1.f" } }

// In a subclass of B the pair (B.f, A.f) is not looked at again, as B inherits both. A.f must still be
// paired with the member of a new parent that it implements: Q needs the bridge f(Seq)CharSequence.
trait X { def f(xs: Seq[String]): CharSequence }
class Q extends B with X

// J.f is seen as f(xs: Array[T with Object]), which C.f does not match: `override` is rejected here.
class C extends J[AnyRef] {
  def f(xs: Array[AnyRef]): String = "C.f"
}
class D extends J[String] {
  def f(xs: Array[String]): String = "D.f"
}

// overriding pairs still get their bridges
abstract class O[T] {
  def f(xs: T*): Object
  def g(x: => T): Object
  def h(x: T)(y: Int): Object
}
class P extends O[String] {
  def f(xs: String*): String = "P.f(String*)"
  def g(x: => String): String = "P.g(=> String)"
  def h(x: String)(y: Int): String = "P.h(String)(Int)"
}

object Test {
  def main(args: Array[String]): Unit = {
    val b = new B
    val a: A = b
    println(a.f(Seq("s")))
    println(b.f(Seq("s")))
    println(b.f("s"))
    println(a.g(() => "s"))
    println(b.g("s"))
    println(a.h("s")(1))
    println(b.h("s", 1))
    println(a.i("s"))
    println(b.i(Seq("s")))
    println(a.j("s"))
    println(b.j(() => "s"))
    println(a.k(Seq("s")))
    println(b.k("s"))

    val ks = new KS
    val k: K[String] = ks
    println(k.f(Seq("s")))
    println(ks.f("s"))
    println(k.g(() => "s"))

    val byName: (=> Int) => Int = x => x
    val function0: (() => Int) => Int = x => x()
    val g1 = new G1
    println((g1: G[(=> Int) => Int]).f(byName))
    println(g1.f(function0))
    val g3 = new G3
    println((g3: G2).f(byName))
    println(g3.f(function0))
    val i1 = new H1.I1
    println((i1: H1.I).f(byName))
    println(i1.f(function0))

    val q = new Q
    println((q: X).f(Seq("s")))
    println((q: A).f(Seq("s")))
    println(q.f("s"))

    val c = new C
    println(c.f(Array[AnyRef]("a")))
    println((c: J[AnyRef]).f(Array[AnyRef]("a")))
    println((new D: J[String]).f(Array("a")))

    val o: O[String] = new P
    println(o.f("s"))
    println(o.g("s"))
    println(o.h("s")(1))
  }
}
