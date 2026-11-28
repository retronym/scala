abstract class AC[@specialized(Long) A] {
  def m(p: Int, q: => A): Int = p
}
class C extends AC[Long] {
  override def m(p: Int, q: => Long): Int = p + 1
}
object Test { def main(a: Array[String]): Unit = println(new C().m(1, 2L)) }
