//> using options -Ycheck:typer -Werror
//
// Re-typing the cast that typer inserts for a GADT-constrained case body must not insert it again.

sealed trait Ty[A]
case object IntTy extends Ty[Int]
case object StrTy extends Ty[String]

object Test {
  def arr[A](t: Ty[A]): Array[A] = t match {
    case IntTy => Array(1, 2)
    case StrTy => Array("a")
  }
  def one[A](t: Ty[A]): A = t match {
    case IntTy => 1
    case StrTy => "a"
  }
}
