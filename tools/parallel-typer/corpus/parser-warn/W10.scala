package pwarn

class W10 {
  def proc() { println(10) }
  val sym = 'sym10
  val long = 10l
  def arrow(xs: List[Int]) = xs.map(y ⇒ y + 10)
  def gen(xs: List[Int]) = for (x ← xs) yield x
  val t = (1, "a", 'c')
  val f: (Int, Int) => Int = _ + _
  @deprecated("old", "1.0") def old = 10
  def use(w: W11) = w.old + w.proc().hashCode
}
