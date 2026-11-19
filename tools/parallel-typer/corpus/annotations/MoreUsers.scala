package ann

class MoreUsers {
  def g: Int = Users.a + Users.d + Old.old
  def h = Users.f
  def i(s: Show[Int]): String = s.show(Holder.N)
}
