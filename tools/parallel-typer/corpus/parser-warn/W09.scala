package pwarn

class W09 {
  def proc() { println(9) }
  val sym = 'sym9
  val long = 9l
  def arrow(xs: List[Int]) = xs.map(y ⇒ y + 9)
  def gen(xs: List[Int]) = for (x ← xs) yield x
  val t = (1, "a", 'c')
  val f: (Int, Int) => Int = _ + _
  @deprecated("old", "1.0") def old = 9
  def use(w: W10) = w.old + w.proc().hashCode
}
