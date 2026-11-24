//> using options -Xasync

import scala.tools.partest.async.OptionAwait._
import org.junit.Assert._

// The receiver of an application is evaluated before its arguments, also when an argument awaits.
object Test {
  class C(var f: Int) {
    def m(a: Int)(b: Int): String = s"m($a)($b)"
    def id: C = this
  }
  class D(val c: C) extends C(0) {
    override def m(a: Int)(b: Int): String = "D." + super.m(a)(b)
    def viaThis: Option[String]  = optionally { m(log("a", 1))(value(Some(log("b", 2)))) }
    def viaSuper: Option[String] = optionally { super.m(log("a", 1))(value(Some(log("b", 2)))) }
    def viaField: Option[String] = optionally { c.m(log("a", 1))(value(Some(log("b", 2)))) }
  }
  object O { def m(a: Int, b: Int): Int = a + b }

  private val sb = new StringBuilder
  def log[T](s: String, t: T): T = { sb.append(s).append(' '); t }
  def check(expected: Any, order: String)(actual: Option[Any]): Unit = {
    val trace = sb.toString.trim
    sb.clear()
    assertEquals(Some(expected), actual)
    assertEquals(order, trace)
  }

  def main(args: Array[String]): Unit = {
    val c = new C(0)

    val it = Iterator(1, 2)
    check(-1, "")(optionally { it.next() - value(Some(it.next())) })
    val it2 = Iterator("a", "b")
    check("ab", "")(optionally { it2.next() + value(Some(it2.next())) })

    var x = 1
    def bump() = { x += 1; x }
    check(3, "")(optionally { x + value(Some(bump())) })
    c.f = 1
    check(11, "")(optionally { c.f + value(Some({ c.f = 5; 10 })) })

    var s = 0
    check(1, "")(optionally { s += value({ s = 10; Some(1) }); s })
    c.f = 0
    check(1, "")(optionally { c.f += value({ c.f = 10; Some(1) }); c.f })

    check("m(1)(2)", "recv a b")(optionally { log("recv", c).m(log("a", 1))(value(Some(log("b", 2)))) })
    check("m(1)(2)", "recv arg")(optionally { log("recv", c).id.id.m(1)(value(Some(log("arg", 2)))) })
    check("m(1)(2)", "r1 r2 arg")(optionally { value(Some(log("r1", c))).m(log("r2", 1))(value(Some(log("arg", 2)))) })
    check("m(1)(2)", "r1 r2 arg")(optionally { log("r2", value(Some(log("r1", c))).id).m(1)(value(Some(log("arg", 2)))) })
    check("m(2)(3)", "a b c")(optionally { new C(log("a", 1)).m(log("b", 2))(value(Some(log("c", 3)))) })
    check(1, "a")(optionally { new C(value(Some(log("a", 1)))).f })
    check(3, "a b")(optionally { O.m(log("a", 1), value(Some(log("b", 2)))) })
    val d = new D(c)
    check("D.m(1)(2)", "a b")(d.viaThis)
    check("m(1)(2)", "a b")(d.viaSuper)
    check("m(1)(2)", "a b")(d.viaField)

    check((), "recv rhs")(optionally { log("recv", c).f = value(Some(log("rhs", 2))) })
    assertEquals(2, c.f)
    val p = new java.awt.Point(0, 0)
    check((), "recv rhs")(optionally { log("recv", p).x = value(Some(log("rhs", 2))) })
    assertEquals(2, p.x)

    val fs = List((x: Int) => x + 1)
    check(2, "fn arg")(optionally { log("fn", fs.head)(value(Some(log("arg", 1)))) })
    val arr = Array(10, 20)
    check(20, "arr i")(optionally { log("arr", arr)(value(Some(log("i", 1)))) })
    check((), "arr i rhs")(optionally { log("arr", arr)(log("i", 0)) = value(Some(log("rhs", 30))) })
    assertEquals(30, arr(0))

    check("123", "x y z")(optionally { "" + log("x", 1) + value(Some(log("y", 2))) + log("z", 3) })
    check(true, "l r")(optionally { log("l", 1) == value(Some(log("r", 1))) })
    check(true, "l r")(optionally { log("l", "a") == value(Some(log("r", "a"))) })
    check(false, "l r")(optionally { log("l", c) eq value(Some(log("r", new C(0)))) })
    check(BigInt(3), "l r")(optionally { log("l", BigInt(1)) + value(Some(log("r", BigInt(2)))) })
    check(List(2, 1), "2 1")(optionally { value(Some(log("2", 2))) :: List(log("1", 1)) })
    check((1, 2), "t1 t2")(optionally { (log("t1", 1), value(Some(log("t2", 2)))) })
  }
}
