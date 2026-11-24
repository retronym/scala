// `C` fixes the specialized type parameter of `B` and overrides a method that is also specialized
// on a type parameter of its own. `B$mcI$sp` has a variant of `sm` for each of the two and one for
// both, `sm$mIcI$sp`, which `C` has to override as well.
class B[@specialized(Int) T] {
  def sm[@specialized(Int) V](u: V, t: T): String = "B.sm"
}
class C extends B[Int] {
  override def sm[@specialized(Int) V](u: V, t: Int): String = "C.sm"
}

class TC extends TB[Double] {
  override def sm[@specialized(Int) V](u: V, t: Double): String = "TC.sm " + u + " " + t
}

// The override is in a trait, which does not get the variant: the class that mixes the trait in does.
class CD extends TD
class CE extends TE
class CF extends TD with TF
class CG extends TG[String]
// Two traits that override the method and are unrelated to each other: one member, for the later one.
class CG2 extends TG[String] with TG2[String]
class CG3 extends TG2[String] with TG[String]
abstract class AD extends TD
class CA extends AD

abstract class AB[@specialized(Int) T] {
  def sm[@specialized(Int) V](u: V, t: T): V
}
class AC extends AB[Int] {
  def sm[@specialized(Int) V](u: V, t: Int): V = u
}

object Test {
  def main(args: Array[String]): Unit = {
    val c = new C
    val b: B[Int] = c
    println(c.sm(1, 2))
    println(b.sm("s", 2))
    println(b.sm(1, 2))

    val tb: TB[Double] = new TC
    println(tb.sm("s", 2.0))
    println(tb.sm(1, 2.0))

    for (x <- List[TB[Double]](new CD, new CE, new CF, new TD {}, new CA)) {
      println(x.sm("s", 2.0))
      println(x.sm(1, 2.0))
    }
    val bg: BG[Int, String] = new CG
    println(bg.sm("s", 2, "x"))
    println(bg.sm(1, 2, "x"))
    for (x <- List[BG[Int, String]](new CG2, new CG3)) {
      println(x.sm("s", 2, "x"))
      println(x.sm(1, 2, "x"))
    }

    val ab: AB[Int] = new AC
    println(ab.sm("s", 2))
    println(ab.sm(1, 2))
  }
}
