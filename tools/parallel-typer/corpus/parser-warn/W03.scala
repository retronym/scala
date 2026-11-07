package pwarn

class W03 {
  def proc() { println(3) }
  val sym = 'sym3
  val long = 3l
  def arrow(xs: List[Int]) = xs.map(y ⇒ y + 3)
  def gen(xs: List[Int]) = for (x ← xs) yield x
  val t = (1, "a", 'c')
  val f: (Int, Int) => Int = _ + _
  @deprecated("old", "1.0") def old = 3
  def use(w: W04) = w.old + w.proc().hashCode
}
