// This test fails: it records that the pattern matcher omits the outer test for a case class
// nested in an object nested in a class, when the pattern's prefix is a val holding that object.
// `x.P(1) == y.P(1)` is false (the synthetic equals tests the outer instance), but `case y.P(_)`
// and `case _: y.P` accept `x.P(1)`. `definedInStaticLocation` follows the prefix of the path
// `y` (a local val) instead of the object's owner, and so treats `y.P` as static.
// The .check is the expected-correct output. Related: scala/bug#4440.
class C { object O { case class P(i: Int) } }

object Test {
  def main(args: Array[String]): Unit = {
    val x = (new C).O
    val y = (new C).O
    val xp: Any = x.P(1)
    println(x.P(1) == y.P(1))
    println(xp match { case y.P(_) => "matched"; case _ => "no match" })
    println(xp match { case _: y.P => "matched"; case _ => "no match" })
  }
}
