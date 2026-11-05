package pwarn

class W14 {
  def proc() { println(14) }
  val sym = 'sym14
  val long = 14l
  def arrow(xs: List[Int]) = xs.map(y ⇒ y + 14)
  def gen(xs: List[Int]) = for (x ← xs) yield x
  val t = (1, "a", 'c')
  val f: (Int, Int) => Int = _ + _
  @deprecated("old", "1.0") def old = 14
  def use(w: W15) = w.old + w.proc().hashCode
}
