import j.JP_1

class C extends JP_1[String]

// Scala permits the companion of a subclass to access a protected static, but the JVM does not.
object C {
  def comp = JP_1.sm(4)
  def field = JP_1.sf
  object Nested { def a = JP_1.sz() }
}
