import java.lang.reflect.{ParameterizedType, Type}

class Foo[T] {
  class Bar[@specialized(Int) U](val u: U) { def t: Option[T] = None; def tu(x: U): Option[(T, U)] = None }
}
object Test {
  // ParameterizedType.toString of a nested type differs between JDK 8 (`Foo<X>.Bar<Y>`) and
  // later (`Foo<X>$Bar<Y>`), so print the owner, raw type and arguments separately.
  def show(t: Type): String = t match {
    case p: ParameterizedType =>
      val owner = if (p.getOwnerType == null) "" else "{" + show(p.getOwnerType) + "}#"
      owner + show(p.getRawType) + p.getActualTypeArguments.map(show).mkString("<", ", ", ">")
    case c: Class[_] => c.getName
    case _ => t.getTypeName
  }

  def main(args: Array[String]): Unit = {
    val foo = new Foo[String]
    for (b <- List[Foo[String]#Bar[_]](new foo.Bar[String]("s"), new foo.Bar[Int](1))) {
      val c = b.getClass
      println(c.getName + " " + (try show(c.getGenericSuperclass) catch { case e: Throwable => e }))
      for (m <- c.getDeclaredMethods.sortBy(_.getName) if m.getName.startsWith("t") && !m.isBridge)
        println("  " + m.getName + ": " + (try show(m.getGenericReturnType) catch { case e: Throwable => e }))
    }
  }
}
