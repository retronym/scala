// `JC.super[J].m` outside `JC` itself needs a super accessor in `JC`: only `JC`
// may `invokespecial` the default method of its direct parent interface `J`.
class JC extends J {
  override def m = "JC>" + super.m
  class I { def k = JC.super[J].m }
  object O { def k = JC.super[J].m }
  def anon = new Runnable { def run() = println(JC.super[J].m) }
  def clo = (() => JC.super[J].m)()
  def byName = Option(1).map(_ => super[J].m).get
}

object Test {
  def main(args: Array[String]): Unit = {
    val jc = new JC
    println(jc.m)
    println(new jc.I().k)
    println(jc.O.k)
    jc.anon.run()
    println(jc.clo)
    println(jc.byName)
  }
}
