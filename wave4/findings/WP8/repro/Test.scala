object Test {
  def m[B](p: B, n: Int): Option[B] = { List(1, 2).foreach(x => if (x > n) return Some(p)); None }
  def main(args: Array[String]): Unit = println(List(m("a", 0), m("b", 5)))
}
