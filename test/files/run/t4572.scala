//> using options -nowarn

object Test {
  def show(f: => Any): Unit = println(try String.valueOf(f) catch { case t: Throwable => t.getClass.getName })
  def same(a: Unit, b: Unit) = a == b
  def main(args: Array[String]): Unit = {
    val us = new Array[Unit](1)
    val u: Unit = us(0)
    show(u == (()))
    show(us(0) == (()))
    show(same(us(0), ()))
    show(us(0).equals(()))
    show(().equals(us(0)))
    show(us(0).toString)
    show(us(0).hashCode)
    show(us.toList == List(()))
    show(new Array[Unit](1).sameElements(Array(())))
  }
}
