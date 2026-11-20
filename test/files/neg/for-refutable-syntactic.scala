// A generator's pattern is treated as refutable unless it is syntactically a variable pattern,
// so `withFilter` is required even where the pattern cannot fail for the generator's element type
// (scala/bug#1336). Pinned as on 2.13.x; Scala 3 checks refutability against the type.
//
// TODO scala/scala review S3: under type-based refutability, `typed` and `extract` compile
// without `withFilter`, so this file would move to pos/ (and the .check go away).
final case class Box(i: Int)

final class NoFilter[A](a: A) {
  def map[B](f: A => B): NoFilter[B] = new NoFilter(f(a))
  def foreach(f: A => Unit): Unit = f(a)
}

object Test {
  def boxes: NoFilter[Box] = new NoFilter(Box(1))
  def ok      = for (b <- boxes) yield b.i
  def typed   = for (b: Box <- boxes) yield b.i
  def extract = for (Box(i) <- boxes) yield i
}
