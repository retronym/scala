object Reg { var log = List.empty[String] }
object O { Reg.log ::= "init O" }
object P { Reg.log ::= "init P" }
class Cfg { var mode = "default"; lazy val snapshot: String = { Reg.log ::= "force snapshot"; mode } }
object Test {
  def arg(): Any = { Reg.log ::= "arg"; "x" }
  def show(label: String)(r: Any): Unit = { println(label + ": " + r + " " + Reg.log.reverse.mkString("[", ", ", "]")); Reg.log = Nil }
  def main(args: Array[String]): Unit = {
    show("isInstanceOf[O.type]")(arg().isInstanceOf[O.type])
    show("match P.type        ")(arg() match { case _: P.type => true; case _ => false })
    val c1, c2 = new Cfg
    // the receiver changes the state that the lazy val reads
    def setThenGet(c: Cfg): Any = { Reg.log ::= "arg"; c.mode = "custom"; c.mode }
    show("isInstanceOf[lz.type]")(setThenGet(c1).isInstanceOf[c1.snapshot.type])
    show("match lz.type        ")(setThenGet(c2) match { case _: c2.snapshot.type => true; case _ => false })
  }
}
