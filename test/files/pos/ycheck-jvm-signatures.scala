//> using options -Ycheck:jvm -Werror

// -Ycheck:jvm compares each generic signature with the descriptor. These signatures are correct; the
// check used to report them as not conforming to their erasure, and dropped them.

class V[A](val a: A) extends AnyVal
class W(val i: Int) extends AnyVal

class C[K <: Comparable[K], E] {
  def v: V[String] = null.asInstanceOf[V[String]]
  def vs(x: List[V[String]]): V[K] = null.asInstanceOf[V[K]]
  def w(l: List[String]): W = new W(l.length)
  def n[T](t: T): Null = null
  def nl(l: List[String]): List[Null] = Nil
  def nothing(l: List[String]): List[Nothing] = Nil
  def k(k: K, l: List[E]): K = k
  def m[M <: K](m: M, l: List[E]): M = m
  def ext(l: List[E]): PartialFunction.ElementWiseExtractor[Int, E] = null.asInstanceOf[PartialFunction.ElementWiseExtractor[Int, E]]
  class Inner[I <: E] {
    def i(e: E, i: I, k: K): List[I] = Nil
  }
  def inner(l: List[E]): Inner[E] = new Inner[E]
  def inners(l: List[E]): List[Inner[E]] = Nil
}

import scala.language.implicitConversions

object O {
  implicit def toV[A](l: List[A]): V[A] = new V(l.head)
  def local[T <: CharSequence](t: T) = {
    class L { def f(l: List[T]): T = t }
    new L
  }
}
