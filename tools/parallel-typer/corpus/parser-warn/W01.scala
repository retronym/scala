package pwarn

class W01 {
  def proc() { println(1) }
  val sym = 'sym1
  val long = 1l
  def arrow(xs: List[Int]) = xs.map(y ⇒ y + 1)
  def gen(xs: List[Int]) = for (x ← xs) yield x
  val t = (1, "a", 'c')
  val f: (Int, Int) => Int = _ + _
  @deprecated("old", "1.0") def old = 1
  def use(w: W02) = w.old + w.proc().hashCode
}
