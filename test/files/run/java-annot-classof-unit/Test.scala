import p.C

object Ob { type U = Unit }

@C(v = classOf[Unit], vs = Array(classOf[Unit], classOf[Int])) class X
@C(v = classOf[Ob.U], vs = Array(classOf[Ob.U], classOf[Int])) class Y

object Test {
  def show(c: C) = s"${c.v} ${c.vs()(0)} ${c.vs()(1)}"
  def main(args: Array[String]): Unit = {
    println("scalac: " + show(classOf[X].getAnnotation(classOf[C])))
    println("alias:  " + show(classOf[Y].getAnnotation(classOf[C])))
    println("javac:  " + show(classOf[JClient.X].getAnnotation(classOf[C])))
  }
}
