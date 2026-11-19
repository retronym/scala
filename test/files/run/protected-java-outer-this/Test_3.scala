import j.JP_1

class D extends B {
  class L extends JP_1[String] { def l = D.this.m() + D.this.f }
  class L2 extends JP_1[String] { def l = { D.this.f += 1; D.this.f } }
}

object Test {
  def main(args: Array[String]): Unit = {
    val b = new B
    val k = new b.K
    println(k.own)
    println(k.k)
    println(k.k3)
    println(k.k4)
    println(k.param(new b.K))
    println(new b.K2().k)
    b.cl.run()
    val d = new D
    println(new d.L().l)
    println(new d.L2().l)
  }
}
