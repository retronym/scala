trait One
trait Two
trait Sub extends One
class K
trait KT extends K
class L extends K

class A {
  // the descriptor has `One`: `Object` is a parent of `One`
  def foo1[T <: Object with One](x: T): String = "foo1"
  def foo2[T <: One with Two](x: T): String = "foo2"
  // erased to `Sub`, a subtype of `One`
  def foo3[T <: One with Sub](x: T): String = "foo3"
  // erased to the trait `KT`, which extends the class `K`
  def foo4[T <: K with KT](x: T): String = "foo4"
  def foo5[T <: Two with K](x: T): String = "foo5"
  // erased to `L`, a subclass of `K`
  def foo6[T <: K with L with One](x: T): String = "foo6"
  def foo7[T <: Object](x: T): String = "foo7"
  def foo8[T <: One](x: T): String = "foo8"
}
