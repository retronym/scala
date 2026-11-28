// A `try` that erasure puts inside the box of a value class (`new C(...)`) runs with the box's
// operands on the stack, which a `catch` handler cannot do. Each method fails verification if so.

case class Meters(value: Double) extends AnyVal
case class Name(value: String) extends AnyVal
case class Box[T](value: T) extends AnyVal
class Done(val u: Unit) extends AnyVal { override def toString = "Done" }

object Test {
  def meters(s: String): Meters = Meters(s.toDouble)

  // A match on types inside a partial function: the match's value is boxed as a whole.
  def collect2(xs: List[Any]): List[Meters] = xs.collect { case x => x match {
    case d: Double => Meters(d)
    case s: String => try meters(s) catch { case _: NumberFormatException => Meters(0) }
  } }

  def collect3(xs: List[Any]): List[Meters] = xs.collect { case x => x match {
    case d: Double => Meters(d)
    case i: Int => Meters(i.toDouble)
    case s: String => try meters(s) catch { case _: NumberFormatException => Meters(0) }
  } }

  // A constant guard keeps the match from becoming a switch.
  def guarded(i: Int): Meters =
    ({ case x => x match {
      case 100 if false => Meters(1)
      case 3 => try Meters(0.5) catch { case _: Exception => Meters(0.6) }
      case _ => Meters(3)
    } }: PartialFunction[Int, Meters]).applyOrElse(i, (_: Int) => Meters(-1))

  def tryFinally(xs: List[Any]): List[Meters] = xs.collect { case x => x match {
    case d: Double => Meters(d)
    case s: String => try meters(s) finally ()
  } }

  def tryCatchFinally(xs: List[Any]): List[Meters] = xs.collect { case x => x match {
    case d: Double => Meters(d)
    case s: String => try meters(s) catch { case _: NumberFormatException => Meters(0) } finally ()
  } }

  // A type ascription in a position of type Any: the ascribed expression is boxed.
  def ascribed(s: String): Any = (try meters(s) catch { case _: NumberFormatException => Meters(0) }): Meters

  def ascribedMatch(x: Any): Any = (x match {
    case d: Double => Meters(d)
    case s: String => try meters(s) catch { case _: NumberFormatException => Meters(0) }
  }): Meters

  def ascribedFinally(s: String): Any = (try meters(s) finally ()): Meters

  def inIf(b: Boolean, s: String): Any =
    (if (b) try meters(s) catch { case _: NumberFormatException => Meters(0) } else Meters(1)): Meters

  def inBlock(s: String): Any = ({ val t = s.trim; try meters(t) catch { case _: NumberFormatException => Meters(0) } }): Meters

  def name(x: Any): Any = (x match {
    case n: Int => Name(n.toString)
    case s: String => try Name(s.substring(1)) catch { case _: StringIndexOutOfBoundsException => Name("?") }
  }): Name

  def box(xs: List[Any]): List[Box[String]] = xs.collect { case x => x match {
    case n: Int => Box(n.toString)
    case s: String => try Box(s.substring(1)) catch { case _: StringIndexOutOfBoundsException => Box("?") }
  } }

  def boxInt(x: Any): Any = (x match {
    case n: Int => Box(n)
    case s: String => try Box(s.toInt) catch { case _: NumberFormatException => Box(0) }
  }): Box[Int]

  def done(b: Boolean): Any = (try new Done(if (b) throw new Exception else ()) catch { case _: Exception => new Done(()) }): Done

  def nested(s: String): Any =
    (try { try meters(s) catch { case _: NumberFormatException => Meters(-1) } } catch { case _: Exception => Meters(0) }): Meters

  // A lambda inside the boxed `try` is owned by the local that holds the value.
  def lambda(s: String): Any = {
    def one = 1.0
    (try { val k = () => one; Meters(s.toDouble + k()) } catch { case _: NumberFormatException => Meters(0) }): Meters
  }

  def main(args: Array[String]): Unit = {
    println(collect2(List(1.5, "2.5", "x")))
    println(collect3(List(1.5, 2, "2.5", "x")))
    println(List(0, 3, 100).map(guarded))
    println(tryFinally(List(1.5, "2.5")))
    println(tryCatchFinally(List(1.5, "2.5", "x")))
    println(List("2.5", "x").map(ascribed))
    println(List(1.5, "2.5", "x").map(ascribedMatch))
    println(ascribedFinally("2.5"))
    println(List(inIf(true, "2.5"), inIf(true, "x"), inIf(false, "x")))
    println(List(" 2.5 ", "x").map(inBlock))
    println(List(1, "ab", "").map(name))
    println(box(List(1, "ab", "")))
    println(List(1, "2", "x").map(boxInt))
    println(List(done(true), done(false)))
    println(List("2.5", "x").map(nested))
    println(List("2.5", "x").map(lambda))
  }
}
