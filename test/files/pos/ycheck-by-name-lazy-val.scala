//> using options -Ycheck:all -Werror

trait T[A] { def z: A; lazy val lz: A = z }
class C[A](val z: A) extends T[A] {
  def by(x: => A): A = x
  def m: A = by(lz)
}
