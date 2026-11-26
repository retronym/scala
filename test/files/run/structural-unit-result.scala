import scala.language.reflectiveCalls

class K {
  def m(x: Int): Unit = ()
  def n: Unit = ()
  def nullString: String = null
  def nullAny: Any = null
}

object Test {
  def show(x: Any): String = if (x == null) "null" else x.toString

  def viaTypeParam[A](r: { def n: A }): A = r.n

  def main(args: Array[String]): Unit = {
    val k = new K
    println(show((k: { def m(x: Int): Any }).m(1)))
    println(show((k: { def n: Any }).n))
    println(show((k: { def n: AnyVal }).n))
    println(show(viaTypeParam[Unit](k)))
    println(show((k: { def n: Unit }).n))
    // a null result from a method that is not void stays null
    println(show((k: { def nullString: Any }).nullString))
    println(show((k: { def nullString: AnyRef }).nullString))
    println(show((k: { def nullAny: Any }).nullAny))
  }
}
