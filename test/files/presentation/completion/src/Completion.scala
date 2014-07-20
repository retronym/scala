object Completion {
  val list = Seq(1,2)
  def add(x: Int, y: Int): Int = x + y
  def minusOne(x: Int) = x - 1


  add(list./*!*/)
  minusOne(list./*!*/)
}
