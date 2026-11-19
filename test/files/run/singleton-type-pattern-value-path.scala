import scala.util.Try

class V(val s: String) extends AnyVal

object Test {
  val i: Int     = 1
  val d: Double  = 1.0
  val c: Char    = 'a'
  val v: V       = new V("v")
  val u: Unit    = ()
  val lit: 1     = 1

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
  // the scrutinee is statically an Int: no type test is needed
  def narrow(x: Int): String = x match {
    case y: i.type => "Int " + (y + 41)
    case _         => "no"
  }
  // numeric scrutinee of another primitive type
  def long(x: Long): String = (x: AnyVal) match {
    case y: i.type => "Int " + (y + 41)
    case _         => "no"
  }

  def show(label: String, f: Any => String, xs: Any*): Unit =
    println(label + xs.map(x => Try(f(x)).fold(_.getClass.getSimpleName, identity)).mkString(": ", ", ", ""))

  def main(args: Array[String]): Unit = {
    val inputs = List[Any](1, 1L, 1.0, 1.0f, BigInt(1), '\u0001', 'a', 97, "1", null)
    show("int", int, inputs: _*)
    show("double", double, inputs: _*)
    show("char", char, inputs: _*)
    show("valueClass", valueClass, new V("v"), new V("w"), "v", null)
    show("unit", unit, (), 1, null)
    show("literal", literal, inputs: _*)
    println("narrow: " + narrow(1) + ", " + narrow(2))
    println("long: " + long(1L))

    // `isInstanceOf` agrees with the pattern
    println("isInstanceOf[i.type]: " + inputs.map(_.isInstanceOf[i.type]).mkString(", "))
    println("isInstanceOf[d.type]: " + inputs.map(_.isInstanceOf[d.type]).mkString(", "))
  }
}
