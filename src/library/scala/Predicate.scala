package scala

final class Predicate[@specialized T](val f: T => Boolean, val isFlipped: Boolean) extends (T => Boolean) {
  def apply(x: T): Boolean = f(x) != isFlipped

  def unary_! : Predicate[T] = f match {
    case x: Predicate[_] if x.isFlipped == isFlipped => x
  }

  def flip: Predicate[T] = f match {
    case x: Predicate[_] => if (isFlipped) x else x.flip
    case _               =>
  }
  def &&(rhs: T => Boolean): T => Boolean = (x: T) => apply(x) && rhs(x)
  def ||(rhs: T => Boolean): T => Boolean = (x: T) => apply(x) || rhs(x)
}

object Predicate {
  class Forward[@specialized T](val f: T => Boolean) extends AnyRef with (T => Boolean) with Predicate[T] {
    def apply(x: T): Boolean = f(x)
    def unary_! : Predicate[T] = f match {
      case x: ForwardPredicate[_] => !x
      case x: FlippedPredicate[_] => x
      case _                      => new FlippedPredicate[T](f)
    }
  }

  class Flipped[@specialized T](val f: ForwardPredicate[T]) extends AnyRef with (T => Boolean) with Predicate[T] {
    def flip: Predicate[T] = f match {
      case x: Predicate[_] => x
      case _               =>
    }
    def apply(x: T): Boolean = !f(x)
    def unary_! : Predicate[T] = f match {
      case x: ForwardPredicate[_] => x
      case x: FlippedPredicate[_] => !x
      case _                      => new ForwardPredicate[T](f)
    }
  }
}

