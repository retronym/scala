//> using options -nowarn
object Reg { var live = Set.empty[String]; def init(s: String): Unit = { println("init " + s); live += s } }
case class Conn(id: Int) { val ok = Reg.live("Conn") }       // reads state its companion sets up
object Conn { Reg.init("Conn") }
object Lib1 { Reg.init("Lib1"); val A = Array }
object Lib2 { Reg.init("Lib2"); val S = Symbol }
object Lib3 { Reg.init("Lib3"); val SC = StringContext }
object Lib6 { Reg.init("Lib6"); val P = Predef }
object Lib0 { Reg.init("Lib0"); val V = Vector }             // control: no rewrite for Vector
object Test {
  def main(args: Array[String]): Unit = {
    val x = 1
    println("Conn(1).ok          = " + Conn(1).ok)            // RefChecks: Conn.apply(1) => new Conn(1)
    println("Lib0.V(1, 2)        = " + Lib0.V(1, 2))
    println("Lib1.A(1, 2).length = " + Lib1.A(1, 2).length)   // CleanUp: Array.apply => array literal
    println("Lib2.S(\"a\")         = " + Lib2.S("a"))         // CleanUp: Symbol.apply => constant
    println("Lib3.SC(..).s(x)    = " + Lib3.SC("a", "b").s(x)) // fast string interpolator
    println("Lib6.P.int2Integer  = " + Lib6.P.int2Integer(x).intValue) // CleanUp: boxing round trip
    println(Reg.live.toList.sorted)
  }
}
