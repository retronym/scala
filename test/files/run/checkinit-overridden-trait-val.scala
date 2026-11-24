//> using options -Xcheckinit

// A trait val overridden in a class: the trait's setter in the class is a no-op,
// so running the trait initializer must not mark the class's field as initialized.

trait T {
  val t = "t"
  def useLater: String
  val u = useLater
}
class Ov extends T { override val t = "ov"; def useLater = t }
class OvLazyRead extends T { override val t = "ov"; lazy val l = t; def useLater = l }

// same shape with a superclass, for comparison
class P { val t = "p"; def useLater: String = ""; val u = useLater }
class Q extends P { override val t = "q"; override def useLater = t }

// two levels of traits
trait T1 { val t = "t1"; def useLater: String; val u = useLater }
trait T2 extends T1 { override val t = "t2" }
class Two extends T2 { def useLater = t }
class TwoOv extends T2 { override val t = "two"; def useLater = t }

// a trait var, read through a class-defined val overriding a trait val
trait V { var v = "v"; val w = "w"; def useLater: String; val u = useLater }
class Vc extends V { override val w = "vc"; def useLater = v + w }

// no false positives: reads after the class constructor ran
class Ok extends T { override val t = "ok"; def useLater = "" }
class OkTwo extends T2 { override val t = "oktwo"; def useLater = "" }
class OkV extends V { override val w = "okv"; def useLater = "" }
trait L { lazy val l = "l"; def useLater: String; val u = useLater }
class OkLazy extends L { override lazy val l = "oklazy"; def useLater = l }

// true positive without an override: the trait reads a field of the class
trait U { def useLater: String; val u = useLater }
class R extends U { val r = "r"; def useLater = r }

object Test {
  def check(name: String)(f: => Any): Unit =
    println(s"$name: " + (try String.valueOf(f) catch { case e: UninitializedFieldError => "UninitializedFieldError" }))

  def main(args: Array[String]): Unit = {
    check("Ov")(new Ov().u)
    check("OvLazyRead")(new OvLazyRead().u)
    check("Q")(new Q().u)
    check("Two")(new Two().u)
    check("TwoOv")(new TwoOv().u)
    check("Vc")(new Vc().u)
    check("R")(new R().u)
    check("Ok")({ val o = new Ok; o.t + " " + o.u })
    check("OkTwo")({ val o = new OkTwo; o.t + " " + o.u })
    check("OkV")({ val o = new OkV; val before = o.v + o.w; o.v = "x"; before + " " + o.v + o.w })
    check("OkLazy")(new OkLazy().u)
  }
}
