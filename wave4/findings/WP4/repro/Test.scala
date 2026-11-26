trait T[A] { def z: A; lazy val lz: A = z }
class C[A](val z: A) extends T[A] {
  def by(x: => A): A = x
  def m: A = by(lz)
  def n: A = (None: Option[A]).getOrElse(lz)
}
object Test {
  def main(args: Array[String]): Unit = { val c = new C("a"); println(List(c.m, c.n)) }
}
