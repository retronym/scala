package pwarn

class W16 {
  def proc() { println(16) }
  val sym = 'sym16
  val long = 16l
  def arrow(xs: List[Int]) = xs.map(y ⇒ y + 16)
  def gen(xs: List[Int]) = for (x ← xs) yield x
  val t = (1, "a", 'c')
  val f: (Int, Int) => Int = _ + _
  @deprecated("old", "1.0") def old = 16
  def use(w: W01) = w.old + w.proc().hashCode
}
