object A { val o: 1 = { println("init A"); 1 } }
object B { val o: 1 = { println("init B"); 1 } }
class C { val o: 1 = 1 }

object Test {
  def main(args: Array[String]): Unit = {
    println(A.o)       // evaluates A.o: prints "init A", then 1
    println(B.o + 1)   // folded to 2: B is never initialised
    val c: C = null
    println(try c.o catch { case _: NullPointerException => "NPE" })
    println(try c.o + 1 catch { case _: NullPointerException => "NPE" })
  }
}
