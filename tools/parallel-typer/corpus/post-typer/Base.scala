package pt

class Foo { type T = String; val t: T = "t" }

class Base {
  def m(x: Int): Int = x
  def gen[A](a: A): A = a
  protected def prot(s: String): String = s
  def over(x: Int): Int = x
  def over(x: String): String = x
}

abstract class AbsBase extends Base {
  def deferred(x: Int): Int
}

trait Stack1 extends Base {
  override def m(x: Int): Int = super.m(x) + 1
  override def gen[A](a: A): A = super.gen(a)
}

trait Stack2 extends Base {
  override def m(x: Int): Int = super.m(x) * 2
  abstract override def over(x: Int): Int = super.over(x) + 2
}

trait Named { def name: String; override def toString: String = "Named(" + name + ")" }
