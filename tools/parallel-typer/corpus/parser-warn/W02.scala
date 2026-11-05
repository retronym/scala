package pwarn

class W02 {
  def proc() { println(2) }
  val sym = 'sym2
  val long = 2l
  def arrow(xs: List[Int]) = xs.map(y ⇒ y + 2)
  def gen(xs: List[Int]) = for (x ← xs) yield x
  val t = (1, "a", 'c')
  val f: (Int, Int) => Int = _ + _
  @deprecated("old", "1.0") def old = 2
  def use(w: W03) = w.old + w.proc().hashCode
}
