// A definition sees the GADT narrowings of the cases enclosing it, however late
// its lazy completer runs. See also neg/gadt-completer-lift.scala.

sealed trait E[T]
case object I extends E[Int]
case object S extends E[String]

sealed trait F[T]
case object J extends F[Int]

class Wrap(i: Int) { def plus = i + 1 }

object Test {
  def localDef[T](e: E[T]): T = e match {
    case I => def h = { val x: T = 1; x }; h
    case S => "s"
  }

  def localClass[T](e: E[T]): T = e match {
    case I => class L { def g = { val x: T = 1; x } }; (new L).g
    case S => "s"
  }

  def caseClass[T](e: E[T]): T = e match {
    case I => case class C(t: T); C(1).t
    case S => "s"
  }

  def insideLambda[T](e: E[T]): List[T] = e match {
    case I => List(1).map { i => def h = { val x: T = i; x }; h }
    case S => Nil
  }

  def nestedCases[T, U](e: E[T], f: F[U]): T = e match {
    case I =>
      def h = { val x: T = 1; x }
      f match { case J => h; case _ => h }
    case S => "s"
  }

  def bothNarrowings[T, U](e: E[T], f: F[U]): Unit = e match {
    case I =>
      f match {
        case J =>
          def h = { val x: T = 1; val y: U = 2; y }
          h
        case _ =>
      }
    case _ =>
  }

  def explicitForwardReference[T](e: E[T]): T = e match {
    case I => def r = h; def h: T = { val x: T = 1; x }; r
    case S => "s"
  }

  def inferredForwardReference[T](e: E[T]): T = e match {
    case I => def k = h; def h = { val x: T = 1; x }; k
    case S => "s"
  }

  def localImplicit[T](e: E[T]): T = e match {
    case I => implicit val i: T = 1; def h = implicitly[T]; h
    case S => "s"
  }

  implicit def wrap(i: Int): Wrap = new Wrap(i)
  def viewInSilentRetry[T](e: E[T], t: T): Int = e match {
    case I => def h = t.plus; h
    case S => 0
  }

  // Forced inside the case, but the narrowing isn't needed for the signature.
  def outsideWithExplicitType[T](e: E[T], t: T): T = {
    e match { case I => h; case _ => () }
    def h: T = t
    h
  }

  // `h` is outside the first case, and narrows `T` afresh in its own.
  def outsideWithOwnCase[T](e: E[T]): Unit = {
    e match { case I => h; val x: T = 1; x; case _ => () }
    def h = e match { case I => val x: T = 1; x; case S => "s" }
    ()
  }

  def restoredOnceComplete[T](e: E[T], t: T): Int = {
    def h = 1
    e match { case I => h + t; case _ => 0 }
  }
}
