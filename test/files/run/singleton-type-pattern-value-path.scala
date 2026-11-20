import scala.util.Try

class V(val s: String) extends AnyVal

class Outer(val o: Int) {
  def member(x: Any): String = x match {
    case y: o.type => "o " + (y + 41)
    case _         => "no"
  }
  class Inner {
    def member(x: Any): String = x match {
      case y: o.type => "o " + (y + 41)
      case _         => "no"
    }
  }
}

object Test {
  val i: Int     = 1
  val d: Double  = 1.0
  val c: Char    = 'a'
  val v: V       = new V("v")
  val u: Unit    = ()
  val lit: 1     = 1
  val s: String  = new String("s")

  // The pattern `y: p.type` is tested with `p == x` for a path that isn't an `AnyRef`. That is cooperative
  // for boxed numbers and chars, so `y` is bound to `p`, the only value of `p.type`, and not to the scrutinee,
  // which may be a box of another class.
  def int(x: Any): String = x match {
    case y: i.type => "Int " + (y + 41)
    case _         => "no"
  }
  def double(x: Any): String = x match {
    case y: d.type => "Double " + (y * 2)
    case _         => "no"
  }
  def char(x: Any): String = x match {
    case y: c.type => "Char " + y.toUpper
    case _         => "no"
  }
  def valueClass(x: Any): String = x match {
    case y: v.type => "V " + y.s
    case _         => "no"
  }
  def unit(x: Any): String = x match {
    case y: u.type => "Unit " + y
    case _         => "no"
  }
  // a path of literal type is still a path, and its binder is narrowed
  def literal(x: Any): String = x match {
    case y: lit.type => "1 " + ((y: Int) + 41)
    case _           => "no"
  }
  // the scrutinee is statically an Int: it is the binder
  def narrow(x: Int): String = x match {
    case y: i.type => "Int " + (y + 41)
    case _         => "no"
  }
  // numeric scrutinee of another primitive type
  def long(x: Long): String = (x: AnyVal) match {
    case y: i.type => "Int " + (y + 41)
    case _         => "no"
  }
  def guard(x: Any): String = x match {
    case y: i.type if y + 41 == 42 => "guard " + (y + 41)
    case _                         => "no"
  }
  def nested(x: Any): String = x match {
    case Some(y: i.type)        => "Some " + (y + 41)
    case (y: i.type, z: c.type) => "Tuple " + (y + 41) + z.toUpper
    case _                      => "no"
  }
  def bind(x: Any): String = x match {
    case y @ (_: i.type) => "bind " + (y + 41)
    case _               => "no"
  }
  def alternative(x: Any): String = x match {
    case _: i.type | _: c.type => "alt"
    case _                     => "no"
  }
  def wildcard(x: Any): String = x match {
    case _: i.type => "yes"
    case _         => "no"
  }
  def local(x: Any): String = {
    val l: Long = 1L
    x match {
      case y: l.type => "Long " + (y + 41L)
      case _         => "no"
    }
  }
  // a path that is an `AnyRef` is tested with `eq`, and the binder is the scrutinee
  def string(x: Any): String = x match {
    case y: s.type => "String " + y.length
    case _         => "no"
  }
  // a path of an abstract type is tested with `==`, and the binder is the scrutinee
  def abstractType[T](t: T, x: Any): String = x match {
    case y: t.type => "T " + y + " " + (y: Any).getClass.getSimpleName
    case _         => "no"
  }

  // a path of an abstract type bounded by a primitive type erases to the primitive: the binder is the path
  def bounded[B <: Int](b: B, x: Any): String = x match {
    case y: b.type => "B " + (y + 41)
    case _         => "no"
  }
  def boundedTwice[C <: Int, B <: C](b: B, x: Any): String = x match {
    case y: b.type => "B " + (y + 41)
    case _         => "no"
  }

  def show(label: String, f: Any => String, xs: Any*): Unit =
    println(label + xs.map(x => Try(f(x)).fold(_.getClass.getSimpleName, identity)).mkString(": ", ", ", ""))

  def main(args: Array[String]): Unit = {
    val inputs = List[Any](1, 1L, 1.0, 1.0f, BigInt(1), '\u0001', 'a', 97, 2, 1.5, "1", null)
    show("int", int, inputs: _*)
    show("double", double, inputs: _*)
    show("char", char, inputs: _*)
    show("valueClass", valueClass, new V("v"), new V("w"), "v", null)
    show("unit", unit, (), 1, null)
    show("literal", literal, inputs: _*)
    println("narrow: " + narrow(1) + ", " + narrow(2))
    println("long: " + long(1L) + ", " + long(2L))
    show("guard", guard, inputs: _*)
    show("nested", nested, Some(1), Some(1L), Some(2), (1.0, 97), (1, 'b'), None, null)
    show("bind", bind, inputs: _*)
    show("alternative", alternative, inputs: _*)
    show("wildcard", wildcard, inputs: _*)
    show("local", local, inputs: _*)
    val outer = new Outer(1)
    show("member", outer.member, inputs: _*)
    show("inner member", (new outer.Inner).member, inputs: _*)
    show("string", string, s, new String("s"), "s", 1, null)
    show("abstractType", abstractType(1, _), 1, 1L, 2, null)
    show("bounded", bounded(1, _), inputs: _*)
    show("boundedTwice", boundedTwice(1, _), inputs: _*)
  }
}
