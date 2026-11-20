//> using options -Xcheckinit

// The specialized subclass runs the generic class's constructor statements,
// which assign fields whose init bits live in the generic class's bitmap.

class Sp[@specialized(Int) A](val a: A) { val y = a.toString }

class Sp2[@specialized(Int) A](a: A) { val x: A = a; val y = "s"; var z = 1 }

class Sp3[@specialized(Int) A](a: A) {
  val x: A = a
  val early = later
  val s = "s"
  def later: String = s
}

// Fields added by specialization (`x$mcI$sp`) have no init bit, so reading one early is not detected
// in the specialized class: Sp4[Int] reads 0, Sp4[String] throws.
class Sp4[@specialized(Int) A](a: A) { val early = later; val x: A = a; def later = x.toString }

object Test {
  def check(name: String)(f: => Any): Unit =
    println(s"$name: " + (try String.valueOf(f) catch { case e: UninitializedFieldError => "UninitializedFieldError" }))

  def main(args: Array[String]): Unit = {
    check("Sp[Int]")(new Sp(1).y)
    check("Sp[String]")(new Sp("a").y)
    check("Sp2[Int]")({ val s = new Sp2(1); s"${s.x} ${s.y} ${s.z}" })
    check("Sp2[String]")({ val s = new Sp2("a"); s"${s.x} ${s.y} ${s.z}" })
    check("Sp3[Int]")(new Sp3(1).early)
    check("Sp3[String]")(new Sp3("a").early)
    check("Sp4[Int]")(new Sp4(1).early)
    check("Sp4[String]")(new Sp4("a").early)
  }
}
