package pwarn

class W13 {
  def proc() { println(13) }
  val sym = 'sym13
  val long = 13l
  def arrow(xs: List[Int]) = xs.map(y ⇒ y + 13)
  def gen(xs: List[Int]) = for (x ← xs) yield x
  val t = (1, "a", 'c')
  val f: (Int, Int) => Int = _ + _
  @deprecated("old", "1.0") def old = 13
  def use(w: W14) = w.old + w.proc().hashCode
}
