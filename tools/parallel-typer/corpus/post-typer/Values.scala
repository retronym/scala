package pt

import scala.annotation.tailrec

// explicit companion
class Meters(val value: Double) extends AnyVal {
  def +(that: Meters): Meters = new Meters(value + that.value)
  def scale(k: Double): Meters = new Meters(value * k)
  def scale(k: Int): Meters = new Meters(value * k)
  override def toString: String = value.toString + "m"
}
object Meters {
  val Zero: Meters = new Meters(0)
  def of(d: Double): Meters = new Meters(d)
}

// synthesised companion, private parameter accessor (made not private by extmethods)
class Id(private val raw: Long) extends AnyVal {
  def next: Id = new Id(raw + 1)
  def show: String = "#" + raw
  @tailrec final def countdown(acc: Int): Int = if (acc <= 0) acc else countdown(acc - 1)
}

trait Showable extends Any { def show: String; def shown: String = "<" + show + ">" }

class Wrapped[A](val unwrap: A) extends AnyVal with Showable {
  def map[B](f: A => B): Wrapped[B] = new Wrapped(f(unwrap))
  def zip[B](that: Wrapped[B]): Wrapped[(A, B)] = new Wrapped((unwrap, that.unwrap))
  def show: String = String.valueOf(unwrap)
  def fold[B](z: B)(f: (B, A) => B): B = f(z, unwrap)
}

object Syntax {
  implicit class RichInt(private val self: Int) extends AnyVal {
    def meters: Meters = new Meters(self.toDouble)
    def times[A](a: => A): List[A] = List.fill(self)(a)
  }
  implicit final class RichString(val s: String) extends AnyVal {
    def id: Id = new Id(s.length.toLong)
    def wrapped: Wrapped[String] = new Wrapped(s)
  }
}
