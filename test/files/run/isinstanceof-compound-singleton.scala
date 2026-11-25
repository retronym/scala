//> using options -nowarn

trait Marker
object P { def name = "P" }
object Q { def name = "Q" }

object Test {
  type QA = Q.type with AnyRef
  def show(label: String)(body: => Any): Unit =
    println(label + " = " + (try String.valueOf(body) catch { case t: Throwable => t.getClass.getName }))

  def main(args: Array[String]): Unit = {
    val any: Any = Q
    val q: Q.type = Q
    // Q does not extend Marker and is not P
    show("any.isInstanceOf[Q.type with Marker]")(any.isInstanceOf[Q.type with Marker])
    show("any.isInstanceOf[Marker with Q.type]")(any.isInstanceOf[Marker with Q.type])
    show("any.isInstanceOf[Q.type with P.type]")(any.isInstanceOf[Q.type with P.type])
    show("any match { case _: Q.type with Marker }")(any match { case _: Q.type with Marker => "match"; case _ => "no match" })
    // the singleton is still tested when it is the only parent that needs a test, or sits behind an alias
    show("any.isInstanceOf[Q.type with AnyRef]")(any.isInstanceOf[Q.type with AnyRef])
    show("(P: Any).isInstanceOf[Q.type with AnyRef]")((P: Any).isInstanceOf[Q.type with AnyRef])
    show("any.isInstanceOf[Marker with QA]")(any.isInstanceOf[Marker with QA])
    show("any.isInstanceOf[QA]")(any.isInstanceOf[QA])
    // null is not an instance, also where the static type of the receiver conforms to every parent
    val nul: Q.type = null
    show("nul.isInstanceOf[Q.type with AnyRef]")(nul.isInstanceOf[Q.type with AnyRef])
    show("nul.isInstanceOf[QA with AnyRef]")(nul.isInstanceOf[QA with AnyRef])
    show("q.isInstanceOf[Q.type with AnyRef]")(q.isInstanceOf[Q.type with AnyRef])
    // a plain singleton type pattern on a scrutinee of another singleton type
    show("q.isInstanceOf[P.type]")(q.isInstanceOf[P.type])
    show("q match { case _: P.type }")(q match { case _: P.type => "P"; case _ => "other" })
    show("q match { case p: P.type => p.name }")(q match { case p: P.type => p.name; case _ => "other" })
  }
}
