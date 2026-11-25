//> using options -nowarn
class VS(val x: String) extends AnyVal

object Other {
  // Never called. Its presence makes the class fail verification.
  def unused(n: Null): String = n.asInstanceOf[VS].x
  def hello(): String = "hello from Other"
}

object Test {
  def show(label: String)(body: => Any): Unit =
    println(label + " = " + (try String.valueOf(body) catch { case t: Throwable => t.getClass.getName }))

  def main(args: Array[String]): Unit = {
    // reference: the same cast on a null of static type Any or String, and on the literal
    val a: Any = null
    val s: String = null
    show("(null: Any).asInstanceOf[VS].x")(a.asInstanceOf[VS].x)
    show("(null: String).asInstanceOf[VS].x")(s.asInstanceOf[VS].x)
    show("null.asInstanceOf[VS].x")(null.asInstanceOf[VS].x)
    // a receiver of static type Null that is not the literal
    show("Other.hello()")(Other.hello())
  }
}
