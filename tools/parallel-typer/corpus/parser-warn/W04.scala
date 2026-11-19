package pwarn

class W04 {
  def proc() { println(4) }
  val sym = 'sym4
  val long = 4l
  def arrow(xs: List[Int]) = xs.map(y ⇒ y + 4)
  def gen(xs: List[Int]) = for (x ← xs) yield x
  val t = (1, "a", 'c')
  val f: (Int, Int) => Int = _ + _
  @deprecated("old", "1.0") def old = 4
  def use(w: W05) = w.old + w.proc().hashCode
}
