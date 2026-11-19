import j.JP_1

class D extends C {
  class Inner2 { def a = JP_1.sm(5) + JP_1.sf }
}

object Test {
  def main(args: Array[String]): Unit = {
    val c = new C
    val i = new c.Inner
    println(c.direct)
    println(i.a)
    println(i.b)
    println(i.c)
    println(c.O.a)
    c.anon.run()
    println(c.byName)
    val d = new D
    println(new d.Inner2().a)
  }
}
