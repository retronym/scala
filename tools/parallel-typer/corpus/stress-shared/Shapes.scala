package stress

sealed trait Shape
final case class Circle(r: Double) extends Shape
final case class Rect(w: Double, h: Double) extends Shape
case object Empty extends Shape
sealed abstract class Tree[+A]
final case class Leaf[A](a: A) extends Tree[A]
final case class Node[A](l: Tree[A], r: Tree[A]) extends Tree[A]
