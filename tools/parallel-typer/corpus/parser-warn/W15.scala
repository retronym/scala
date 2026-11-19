package pwarn

class W15 {
  def proc() { println(15) }
  val sym = 'sym15
  val long = 15l
  def arrow(xs: List[Int]) = xs.map(y ⇒ y + 15)
  def gen(xs: List[Int]) = for (x ← xs) yield x
  val t = (1, "a", 'c')
  val f: (Int, Int) => Int = _ + _
  @deprecated("old", "1.0") def old = 15
  def use(w: W16) = w.old + w.proc().hashCode
}
