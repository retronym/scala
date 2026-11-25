// Compiled separately from the classes that mix these traits in.
trait TB[@specialized(Int, Double) T] {
  def sm[@specialized(Int) V](u: V, t: T): String = "TB.sm"
}
trait TD extends TB[Double] {
  override def sm[@specialized(Int) V](u: V, t: Double): String = "TD.sm " + u + " " + t
}
trait TE extends TD
trait TF extends TD {
  override def sm[@specialized(Int) V](u: V, t: Double): String = "TF.sm " + u + " " + t
}

trait BG[@specialized(Int) T, X] {
  def sm[@specialized(Int) V](u: V, t: T, x: X): String = "BG.sm"
}
trait TG[X] extends BG[Int, X] {
  override def sm[@specialized(Int) V](u: V, t: Int, x: X): String = "TG.sm " + u + " " + t + " " + x
}
trait TG2[X] extends BG[Int, X] {
  override def sm[@specialized(Int) V](u: V, t: Int, x: X): String = "TG2.sm " + u + " " + t + " " + x
}
