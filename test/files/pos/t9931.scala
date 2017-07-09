trait Compute[A] {
  type Start
  val start: Compute[Start]
}

object Test {
  def foo[A](c: Compute[A]): Unit = {
    // class Base extends Compute[A] {
    //   type Start = c.Start
    //   val start = c.start
    // }
    c match {
      case c: Compute[a] =>
        class Base extends Compute[A] {
          type Start = c.Start
          val start = c.start
        }
    }
  }
}
