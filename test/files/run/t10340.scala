class D { lazy val x: Int = { println("  D.x initialiser ran"); 1 } }
trait TL { lazy val y: String = { println("  TL.y initialiser ran"); "y" } }
object O { lazy val z: Int = { println("  O.z initialiser ran"); 1 } }
object P { println("  P initialiser ran") }


// A val that a lazy val or an object implements; its symbol is neither lazy nor a module.
trait AbstractVal { val v: Any }
class LazyImpl extends AbstractVal { lazy val v: Int = { println("  LazyImpl.v initialiser ran"); 1 } }
class ObjectImpl extends AbstractVal { object v { println("  ObjectImpl.v initialiser ran") } }
class Plain { val x: Int = 1 }

class Constants {
  final val folded = { val a = 1; val b = a; lazy val c = P; 2 }
  final val kept = { val p = P; 3 }
}

object Test {
  // The val forces the lazy val; the block then returns a literal.
  def force1(d: D): Boolean = { val forced = d.x; true }
  def force2(t: TL): String = { val forced = t.y; "done" }
  def force3(): Int = { val forced = O.z; 0 }
  def force4(): Int = {
    lazy val w: Int = { println("  local w initialiser ran"); 1 }
    def go(): Int = { val forced = w; 0 }
    go()
  }
  // The val initialises the object.
  def force5(): Int = { val forced = P; 0 }
  def force6(a: AbstractVal): Int = { val forced = a.v; 0 }
  // The val reads through a null receiver.
  def force7(p: Plain): Int = { val forced = p.x; 0 }
  // Same blocks with a non-literal result, or with a statement instead of a val.
  def ok1(d: D, r: Boolean): Boolean = { val forced = d.x; r }
  def ok2(d: D): Boolean = { d.x; true }

  def main(args: Array[String]): Unit = {
    println("force1"); force1(new D)
    println("force2"); force2(new TL {})
    println("force3"); force3()
    println("force4"); force4()
    println("force5"); force5()
    println("force6"); force6(new LazyImpl); force6(new ObjectImpl)
    println("force7"); println("  " + (try force7(null) catch { case _: NullPointerException => "NullPointerException" }))
    println("ok1"); ok1(new D, true)
    println("ok2"); ok2(new D)
    // A block of local definitions is still a constant expression: `folded` needs no field.
    println("fields: " + classOf[Constants].getDeclaredFields.map(_.getName).mkString(", "))
  }
}
