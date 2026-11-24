import j.JP_1

// Accessors for protected members of a generic Java class, from closures and nested classes.
class C extends JP_1[String] {
  def clo = { val i = () => { tf = "tf"; tf + m("m") + g(1, "t") }; i() }
  class I { def a = { tf = "i"; tf + m("x") + g(2, "u") } }
}

// The accessors are in the generic `D`; the qualifiers need not be `D[A]`.
class D[A] extends JP_1[A] {
  class I {
    def get(o: D[Int]) = o.tf
    def set(o: D[Int]) = { o.tf = 42; o.tf + 1 }
    def mm(o: D[Int]) = o.m(3) + 1
    def gg(o: D[Int]) = o.g("s", 4)
    def ex(o: D[_]) = o.tf
    def exSet(o: D[_ >: String]) = { o.tf = "ex"; o.tf }
    def own(a: A) = { tf = a; m(tf) }
    def own2 = g(1, tf)
  }
  def clo(a: A) = (() => { tf = a; m(tf) })()
}

object Test {
  def main(args: Array[String]): Unit = {
    println(new C().clo)
    val c = new C
    println(new c.I().a)
    val d = new D[String]
    val i = new d.I
    val di = new D[Int]
    println(i.set(di))
    println(i.get(di))
    println(i.mm(di))
    println(i.gg(di))
    println(i.ex(di))
    println(i.exSet(d))
    println(i.own("own"))
    println(i.own2)
    println(d.clo("clo"))
  }
}
