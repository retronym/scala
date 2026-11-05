package pwarn

class W11 {
  def proc() { println(11) }
  val sym = 'sym11
  val long = 11l
  def arrow(xs: List[Int]) = xs.map(y ⇒ y + 11)
  def gen(xs: List[Int]) = for (x ← xs) yield x
  val t = (1, "a", 'c')
  val f: (Int, Int) => Int = _ + _
  @deprecated("old", "1.0") def old = 11
  def use(w: W12) = w.old + w.proc().hashCode
}
