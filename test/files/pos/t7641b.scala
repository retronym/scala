trait PPrint[A]

object Test {
  def foo[A: PPrint](a: A) = a
  def test1[A: PPrint](a: A) = Seq(a).foreach(foo(_))
  def test2[A: PPrint](a: A) = Seq(a).foreach(foo _) // was failing
}
