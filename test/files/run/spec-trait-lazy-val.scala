// The lazy val initialiser that the fields phase synthesises in a class implementing a specialized trait
// calls `super.lz`. With the trait implemented at a primitive type, that super call used to reach the
// trait's generic-to-specialized forwarder, which dispatched back to the class's lazy val and overflowed
// the stack.

trait TV[@specialized(Int) T] {
  def v: T
  lazy val lz: T = { println("init lz"); v }
}
class TVI extends TV[Int] { def v = 1 }
class TVG[@specialized(Int) T](val v: T) extends TV[T]
class TVP[T](val v: T) extends TV[T]

object Test {
  def t(label: String)(f: => Any): Unit =
    try println(s"$label: $f")
    catch { case _: StackOverflowError => println(s"$label: StackOverflowError") }

  def main(args: Array[String]): Unit = {
    t("class") { val x = new TVI; (x.lz, x.lz) }
    t("class, via trait") { val x: TV[Int] = new TVI; (x.lz, x.lz) }
    t("anonymous")(new TV[Int] { def v = 2 }.lz)
    t("specialized generic class, Int")(new TVG(3).lz)
    t("specialized generic class, String")(new TVG("s").lz)
    t("generic class, Int")(new TVP(4).lz)
    t("generic class, String")(new TVP("p").lz)
  }
}
