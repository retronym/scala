class K2 extends JC

class K3 extends JC {
  def m(x: Int, y: Int): String = "K3.m(" + x + ", " + y + ")"
}

object Test {
  def main(args: Array[String]): Unit = {
    val k1 = new K1
    println((k1: H1).m(1))
    println((k1: H2).m("s"))
    val k2 = new K2
    println((k2: H1).m(1))
    println((k2: H2).m("s"))
    println(k2.m("t"))
    val k3 = new K3
    println((k3: H1).m(1))
    println((k3: H2).m("s"))
    println(k3.m(1, 2))
  }
}
