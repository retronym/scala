// canEqual of a case value class must test for the value class, not its underlying type
case class VS(x: String) extends AnyVal
case class VI(x: Int) extends AnyVal
case class PS(x: String)

object Test {
  // Erasure splits a test for an intersection into tests built with Object_isInstanceOf, as canEqual is.
  def isVS(x: Any): Boolean = x.isInstanceOf[VS with Serializable]
  def matchVS(x: Any): String = x match { case v: VS with Serializable => v.x; case _ => "no" }
  def t(f: => Any): Unit = println(try f catch { case e: Throwable => e.getClass.getName })

  def main(args: Array[String]): Unit = {
    val v = VS("a")
    println(v.canEqual(VS("b")))
    println(v.canEqual("a"))
    println(VI(1).canEqual(VI(2)))
    println(VI(1).canEqual(1))
    println((v: Product).canEqual(VS("b")))
    println((v: Product).canEqual("a"))
    // a plain case class, for comparison
    println(PS("a").canEqual(PS("b")))
    println(PS("a").canEqual("a"))
    t(isVS(VS("a")))
    t(isVS("a"))
    t(matchVS(VS("a")))
    t(matchVS("a"))
  }
}
