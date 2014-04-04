object shapeless {
  trait Iso[A, B]
  trait LowPriorityIso {
    implicit def identityIso[T]: Iso[T, T] = ???
  }
  object Iso extends LowPriorityIso{
    implicit def tupleHListIo[T <: Product, L <: HList](implicit hl : HListerAux[T, L]): Iso[T, L] = ???
  }

  sealed trait HList
  final case class ::[+H, +T <: HList](head : H, tail : T) extends HList
  trait HNil extends HList {
    def ::[H](h : H): H :: HNil = ???
  }
  object HNil extends HNil

  trait HListerAux[-T <: Product, Out <: HList]
  object HListerAux {
    implicit def tupleHLister1[A]: HListerAux[Product1[A], A :: HNil] = ???
  }
}


object DeepSearchExamples {

  // Evidence that an A is something that we can look around in for Qs that
  // satisfy some predicate.
  trait Searchable[A, Q]

  import shapeless._
  trait LowPriorityImplicits1 {
    implicit def hlistishSearchable[A, L <: HList, Q](implicit iso: Iso[A, L], s: Searchable[L, Q]): Searchable[A, Q] = ???
  }
  trait LowPriorityImplicits0 extends LowPriorityImplicits1 {
    implicit def hlistSearchable[H, T <: HList, Q](implicit hs: Searchable[H, Q], ts: Searchable[T, Q]): Searchable[H :: T, Q] = ???
  }
  object Implicits extends LowPriorityImplicits0 {
    implicit def elemSearchable[A]: Searchable[A, A] = ???
    implicit def hnilSearchable[Q]: Searchable[HNil, Q] = ???
  }
  import Implicits._

  implicitly[Searchable[Tuple1[Tuple1[String]], String]]
}

