import scala.annotation.varargs

object S {
  @varargs def s[B <: CharSequence](bs: B*): Int = bs.length
}

object Test {
  def t(label: String)(f: => Int): Unit =
    try println(s"$label: $f")
    catch { case e: ClassCastException => println(s"$label: $e") }

  def viaTypeParam[T <: CharSequence](c: C[T]) = c.f()

  def main(args: Array[String]): Unit = {
    t("f()")(V.f())
    t("f[Nothing]()")(V.f[Nothing]())
    t("f(null)")(V.f(null))
    t("f(Nil: _*)")(V.f(Nil: _*))
    t("f(Seq(): _*)")(V.f(Seq(): _*))
    t("f(Seq.empty[String]: _*)")(V.f(Seq.empty[String]: _*))
    t("f(\"a\", sb)")(V.f("a", new java.lang.StringBuilder("b")))
    t("f(Seq(\"a\", sb): _*)")(V.f(Seq("a", new java.lang.StringBuilder("b")): _*))
    t("g(\"s\")")(V.g("s"))
    t("g(\"s\", Nil: _*)")(V.g("s", Nil: _*))
    t("g(\"s\", 1, 2)")(V.g("s", Int.box(1), Int.box(2)))
    t("h()")(V.h())
    t("h(Nil: _*)")(V.h(Nil: _*))
    t("p()")(V.p())
    t("p(Nil: _*)")(V.p(Nil: _*))
    t("new C[String].f()")(new C[String].f())
    t("new C[Nothing].f()")(new C[Nothing].f())
    t("new C[Nothing].f(Nil: _*)")(new C[Nothing].f(Nil: _*))
    t("viaTypeParam")(viaTypeParam(new C[String]))
    t("S.s()")(S.s())
    t("V.callScala")(V.callScala())
  }
}
