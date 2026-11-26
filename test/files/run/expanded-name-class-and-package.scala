class p { trait T { private lazy val x = "class p"; private var y = "class p"; def gc = (() => x + " " + y)() } }
package p { trait T { private lazy val x = "package p"; private var y = "package p"; def gp = (() => x + " " + y)() } }
object Test {
  def main(args: Array[String]): Unit = {
    val o = new p
    class C extends o.T with p.T
    val c = new C
    println(c.gc)
    println(c.gp)
  }
}
