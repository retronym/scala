class Test {
  val r = new R
  def receiver: Unit = r.receiver(1)
  def only: Unit = r.only()
  def annotated: Unit = r.annotated("")
  def generic: String = r.generic("", "", "")

  def inner0: R#Inner = new r.Inner()
  def inner1: R#Inner = new r.Inner(1)
  def inner2: R#Inner = new r.Inner("")
  def m: Unit = inner0.m(1)
  def n: Unit = inner0.n()

  val g = new R.G[String]
  def gm: Unit = g.m("")
  def gi: R.G[String]#I = new g.I("")
}
