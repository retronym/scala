sealed trait Tag[A]
case object LS extends Tag[List[String]]

object Test {
  // base types of `T` computed before the match must not hide the bounds that the case gives it
  def f[T](tag: Tag[T], x: T): Int = {
    val l0 = if (tag ne null) x else Nil
    tag match { case LS => x.map(_.length).sum; case _ => 0 }
  }

  def g[T](tag: Tag[T], x: T): Int = {
    val l0 = if (tag ne null) x else Nil
    tag match {
      case LS => val l = if (tag ne null) x else Nil; l.size
      case _  => 0
    }
  }
}
