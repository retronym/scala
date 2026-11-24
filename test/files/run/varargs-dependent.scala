import scala.annotation.varargs

abstract class A { type T }

object O {
  // the repeated parameter's type depends on a parameter of an earlier parameter list
  @varargs def unbounded(a: A)(xs: a.T*): Int = xs.length
  @varargs def bounded(a: A { type T <: String })(xs: a.T*): String = xs.mkString
  @varargs def chained[X <: A { type T <: String }](a: X)(b: a.type)(xs: b.T*): String = xs.mkString
  @varargs def nested(a: A { type T <: String })(xs: Option[a.T]*): String = xs.flatten.mkString
}

object Test {
  def main(args: Array[String]): Unit = {
    val a = new A { type T = String }
    println(O.unbounded(a)("a", "b"))
    println(O.bounded(a)("a", "b"))
    println(O.chained[A { type T = String }](a)(a)("a", "b"))
    println(O.nested(a)(Some("a"), None))

    def forwarder(name: String) = O.getClass.getMethods.find(m => m.getName == name && m.isVarArgs).get
    println(forwarder("unbounded").invoke(O, a, Array[AnyRef]("a", "b")))
    println(forwarder("bounded").invoke(O, a, Array[String]("a", "b")))
    println(forwarder("chained").invoke(O, a, a, Array[String]("a", "b")))
    println(forwarder("nested").invoke(O, a, Array[Option[_]](Some("a"), None)))
    for (name <- List("unbounded", "bounded", "chained", "nested"))
      println(forwarder(name))
  }
}
