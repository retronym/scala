class V(val a: String) extends AnyVal {
  def foo = a.length
}

trait A {
  val x: Any = 0
}

object V extends A

object Test extends App {
  import java.lang.reflect._
  println(
    classOf[V]
      .getMethods
      .filter(m => Modifier.isStatic(m.getModifiers))
      .map(_.toString)
      .sorted
      .mkString("\n")
  )
}
