// JLS 8.9: an enum class cannot be instantiated, its constructors are private
object Test {
  def color = new Color()
  def num = new Num(5)
  def ok = (Color.RED, Num.ONE, Color.values, Num.valueOf("ONE"))
}
