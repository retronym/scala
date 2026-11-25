import p._

// The client never names `p.Hid`, which is not accessible here. Type inference does, and the
// class must not be named in a checkcast, an array creation or a class literal.
object Test {
  def t(name: String)(f: => Any): Unit =
    try println(name + "=" + f) catch { case e: Throwable => println(name + " throws " + e.getClass.getName) }

  def main(args: Array[String]): Unit = {
    val x = new Pub
    t("val from generic result")({ val y = x.list().get(0); y != null })
    t("tuple pattern")({ val (y, n) = (x.self(), 1); n })
    t("local lazy val")({ lazy val y = x.self(); y != null })
    t("Array.apply")(Array(x.self()).length)
    t("Java varargs")(java.util.Arrays.asList(x.self(), x.self()).size)
    t("ClassTag")(List(x.self()).toArray.length)
  }
}
