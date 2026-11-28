trait T0[A] {
  def m[@specialized(Int) B](p: B): Int = 1
}
trait T1[A] extends T0[A] {
  override def m[@specialized(Int) B](p: B): Int = super.m[B](p) + 1
}
class C extends T1[String]
object Test { def main(a: Array[String]): Unit = println(new C().m(1)) }
