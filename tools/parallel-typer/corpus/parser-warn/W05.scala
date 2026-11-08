package pwarn

class W05 {
  def proc() { println(5) }
  val sym = 'sym5
  val long = 5l
  def arrow(xs: List[Int]) = xs.map(y ⇒ y + 5)
  def gen(xs: List[Int]) = for (x ← xs) yield x
  val t = (1, "a", 'c')
  val f: (Int, Int) => Int = _ + _
  @deprecated("old", "1.0") def old = 5
  def use(w: W06) = w.old + w.proc().hashCode
}
