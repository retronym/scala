//> using options -Ycheck:_ -Werror

// The inferred type argument of `foreachRec` is a skolem of the existential in the type of `slice(i)`.
// Re-typing the argument skolemizes afresh, which -Ycheck used to report as an erroneous type.
object S {
  def foreachRec[T <: AnyRef, A, U](level: Int, a: Array[T], f: A => U): Unit = ()
}
class C {
  def slice(i: Int): Array[_ <: AnyRef] = null
  def f(i: Int) = S.foreachRec(0, slice(i), (x: Int) => x)
  def g(i: Int) = { val s = slice(i); S.foreachRec(0, s, (x: Int) => x) }
  def h(xs: List[_ <: CharSequence]) = xs.map(_.length)
}
