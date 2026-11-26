case class V0(u: Double) extends AnyVal
object Test {
  def m(i: Int): V0 = ({ case x => x match { case 100 if false => V0(1.0); case 3 => try V0(0.5) catch { case _: Exception => V0(0.6) }; case _ => V0(3.0) } }: PartialFunction[Int, V0]).applyOrElse(i, (x: Int) => V0(-1.0))
  def main(a: Array[String]): Unit = println(List(0, 3, 4, 100).map(m))
}
