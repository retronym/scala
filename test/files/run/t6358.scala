class L(val t: Int) extends AnyVal {
  def lazyString = {
    lazy val x = t.toString
    () => x
  }
}

object Test extends App {
  println(new L(123).lazyString())
}
