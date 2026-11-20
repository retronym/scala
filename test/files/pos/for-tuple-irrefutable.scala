// This test fails: it records that a tuple pattern in a generator requires `withFilter` although
// the pattern is irrefutable for the element type, and SLS 6.19 only inserts the filter for a
// pattern that is not irrefutable. See also neg/for-refutable-syntactic and scala/bug#900.
final class NoFilter[A](a: A) {
  def map[B](f: A => B): NoFilter[B] = new NoFilter(f(a))
  def foreach(f: A => Unit): Unit = f(a)
}

object Test {
  def pairs: NoFilter[(Int, String)] = new NoFilter((1, "a"))
  def tuple = for ((i, s) <- pairs) yield s * i
  def either: Either[String, (Int, Int)] = Right((1, 2))
  def sum = for ((x, y) <- either) yield x + y
}
