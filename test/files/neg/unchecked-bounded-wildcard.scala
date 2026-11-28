//> using options -Werror
object Strs {
  // the pattern type is List[T] for some T <: String
  def unapply[T <: String](xs: List[T]): Option[Int] = Some(xs.head.length)
}

object Test {
  type OS = Option[_ <: String]

  def test(o: Option[Any], l: List[Any], s: Option[String], ls: List[String]): Unit = {
    println(o.isInstanceOf[Option[_ <: String]])                               // warn
    println(o match { case x: OS => x.get.length; case _ => -1 })              // warn
    println(l match { case Strs(n) => n; case _ => -1 })                       // warn
    println(o match { case x: Option[String] => x.get.length; case _ => -1 })  // warns today
    // the bound is implied by the scrutinee: no warning
    println(s.isInstanceOf[Option[_ <: String]])
    println(s match { case x: OS => x.get.length; case _ => -1 })
    println(ls match { case Strs(n) => n; case _ => -1 })
    println(o match { case x: Option[_] => x.isEmpty; case _ => false })
  }
}
