// A definition sees the GADT narrowings of the cases enclosing it, not those of
// the case that happened to force its lazy completer.

sealed trait E[T]
case object I extends E[Int]
case object S extends E[String]

sealed trait F[T]
case object J extends F[Int]

class HasM { trait M }
case object H extends E[HasM]

object Test {
  def localDef[T](e: E[T]): T = {
    e match { case I => h; case _ => () }
    def h = { val x: T = 1; x }
    h
  }

  def lazyVal[T](e: E[T]): T = {
    e match { case I => v; case _ => () }
    lazy val v = { val x: T = 1; x }
    v
  }

  def polymorphicDef[T](e: E[T]): T = {
    e match { case I => h[String]; case _ => () }
    def h[U] = { val x: T = 1; x }
    h[String]
  }

  def chainedDef[T](e: E[T]): T = {
    e match { case I => h; case _ => () }
    def h = k
    def k = { val x: T = 1; x }
    h
  }

  def localClassMember[T](e: E[T]): T = {
    e match { case I => (new L).g; case _ => () }
    class L { def g = { val x: T = 1; x } }
    (new L).g
  }

  def localObjectMember[T](e: E[T]): T = {
    e match { case I => O.g; case _ => () }
    object O { def g = { val x: T = 1; x } }
    O.g
  }

  def defaultArgument[T](e: E[T]): T = {
    e match { case I => h(); case _ => () }
    def h(a: T = { val x: T = 1; x }) = a
    h()
  }

  def forcedInGuard[T](e: E[T]): T = {
    e match { case I if { h; true } => (); case _ => () }
    def h = { val x: T = 1; x }
    h
  }

  // The self type is forced separately from the info of the trait.
  def selfType[T](e: E[T], t: T): Any = {
    e match { case H => class X extends L with t.M; case _ => () }
    trait L { self: t.M => }
    1
  }

  // `h` is in the outer case, so it sees `T = Int` but not the inner case's `U = Int`.
  def innerCaseOnly[T, U](e: E[T], f: F[U]): Unit = e match {
    case I =>
      f match { case J => h; case _ => () }
      def h = { val x: T = 1; val y: U = 2; y }
      ()
    case _ =>
  }

  // Lifting the narrowing while `h` completes puts it back for the rest of the case.
  def stillNarrowedAfterForcing[T](e: E[T]): T = {
    e match { case I => h; val ok: T = 1; ok; case _ => () }
    def h = { val x: T = 1; x }
    h
  }
}
