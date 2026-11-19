//> using options -Ycheck:typer,refchecks -Werror

// The tree checker types the trees again, and until patmat they contain `UnApply`.
case class C(x: Int, s: String)

object Pair { def unapply(c: C): Option[(Int, String)] = Some((c.x, c.s)) }
object NameBased {
  class R(val c: C) extends AnyVal { def isEmpty = false; def get = c.x }
  def unapply(c: C): R = new R(c)
}
object Bool { def unapply(c: C): Boolean = c.x > 0 }
object Poly { def unapply[T](xs: List[T]): Option[T] = xs.headOption }

class Test {
  def seq(xs: List[Int]): Int = xs match {
    case List(x)               => x
    case List(x, rest @ _*)    => x + rest.sum
    case Seq(_, _*)            => 1
    case _                     => 0
  }
  def nested(a: Any): Any = a match {
    case Some(List(c @ C(n, _), _*)) if n > 0 => c
    case Some(Pair(n, s: String))             => s * n
    case Some(Pair(1 | 2, "a"))               => 1
    case Some(NameBased(n))                   => n
    case Some(Bool())                         => true
    case (x, List(y: String))                 => (y, x)
    case _                                    => null
  }
  def poly[A](xs: List[List[A]]): Option[A] = xs match {
    case Poly(Poly(a)) => Some(a)
    case _             => None
  }
  def interpolated(s: String): String = s match {
    case s"$key=$value" => value + key
    case _              => s
  }
  val (a, b)       = (1, "")
  val Some(c: Int) = Option(1): @unchecked
  val pf: PartialFunction[Any, Int] = { case Some(Pair(n, _)) => n }
  def tried: Int = try 1 catch { case scala.util.control.NonFatal(e) => e.getMessage.length }
  // existentials in the signature of the method called
  def wild(c: Class[_], xs: List[_ <: AnyRef]): Int = xs.size
  def callWild: Int = wild(classOf[C], Nil) + java.util.Objects.requireNonNullElse[String]("", "").length
  def comprehension(xs: List[Option[(Int, C)]]) = for (Some((i, Pair(n, _))) <- xs) yield i + n
}
