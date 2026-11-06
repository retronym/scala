package lp

sealed trait Shape { def area: Double }
final case class Circle(r: Double) extends Shape { def area = 3.14 * r * r }
final case class Rect(w: Double, h: Double) extends Shape { def area = w * h }
case object Empty extends Shape { def area = 0.0 }
sealed abstract class Tree[+A]
case class Leaf[A](a: A) extends Tree[A]
case class Node[A](l: Tree[A], r: Tree[A]) extends Tree[A]
case object Nope extends Tree[Nothing]
object Extract {
  def unapply(s: Shape): Option[(Double, String)] = Some((s.area, s.toString))
  def unapplySeq(s: String): Option[Seq[String]] = Some(s.split(",").toSeq)
}
class Inner0 { case class In(i: Int); def mk(i: Int) = In(i) }
