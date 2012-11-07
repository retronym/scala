sealed trait KList[+M[_]]

case class KCons[M[_], +T <: KList[M]](
  tail: T
) extends KList[M]

case class KNil[M[_]]() extends KList[M]

object Test {
  val klist: KCons[Option, KCons[Option, KCons[Option, KNil[Nothing]]]] = ???

  // crashes with
  // "Exception in thread "main" scala.reflect.internal.Types$TypeError: value _1 is not a member of Cons[Option,Cons[Option,Nil[Nothing]]]"
  klist match {
   case KCons(KCons(KCons(_))) =>
  }

  // fails, but with another message:
  // error: error during expansion of this match (this is a scalac bug).
  // The underlying error was: value _1 is not a member of List[Option] with Cons[Option,Cons[Option,Nil[Nothing]]]
  klist match {
    case KCons(KCons(_)) =>
  }

  // succeeds
  klist match {
    case KCons(_) =>
  }
}
