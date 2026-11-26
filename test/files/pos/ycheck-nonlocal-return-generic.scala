//> using options -Ycheck:all -Werror

object Test {
  def m[B](p: B, n: Int): Option[B] = { List(1, 2).foreach(x => if (x > n) return Some(p)); None }
}
