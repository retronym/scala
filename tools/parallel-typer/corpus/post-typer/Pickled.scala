package pt

import scala.annotation.{StaticAnnotation, switch}

class tag(val name: String, val weight: Int = 0) extends StaticAnnotation

// what the pickler writes beyond plain signatures: children, annotations with arguments, refinements,
// existentials, constants, a local child, specialised type parameters
sealed trait Shape
case class Circle(r: Double) extends Shape
case class Square(side: Double) extends Shape
case object Empty extends Shape

sealed abstract class Token
object Token {
  final class Word(val s: String) extends Token
  def anonymous: Token = new Token {}
}

class Pickled[@specialized(Int, Double) T](val t: T) {
  @tag("first", weight = 1) def first: T = t
  @deprecated("use first", "0.1") def old: T = t
  @throws(classOf[java.io.IOException]) def risky(): Unit = ()
  final val Const = 42
  final val Text = "text"
  def refined: Named { def extra: Int } = new Named { def name = "r"; def extra = 1 }
  def existential: java.util.List[_ <: Shape] = new java.util.ArrayList[Shape]
  def raw(c: Class[_]): Array[Class[_]] = Array(c)
  type Alias[A] = Map[String, A]
  def aliased: Alias[T] = Map.empty
  def byName(x: => T)(implicit ord: Ordering[T]): T = x
  def sw(i: Int): Int = (i: @switch) match { case 0 => 1; case 1 => 0; case _ => i }
}

@SerialVersionUID(7L)
class Ser(val n: Int) extends Serializable
