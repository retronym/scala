object Test {
  import ParboiledLite._
  implicitly[RunResult[String => Unit]]
  /* 2.10.4
implicitly[RunResult[String => Unit]](
  RunResult.fromAux[String => Unit, Rule[::[String, HNil], HNil]](
    RunResult.Aux.forF1[String, Unit, ::[String, HNil], HNil](
      Join.Aux.forUnit[::[String, HNil], HNil, HNil, HNil, HNil](
        Join.Aux.terminate[::[String, HNil], HNil, HNil, HNil](
          ReversePrepend.hnilReversePrepend1[HNil, HNil])))))
  */
}

object ParboiledLite {
  trait HList
  final case class ::[+H, +T <: HList](head : H, tail : T) extends HList
  trait HNil extends HList
  trait ReversePrepend[P <: HList, S <: HList] { type Out <: HList }

  trait Reverse[T] { type Out }
  object ReversePrepend {
    type Aux[P <: HList, S <: HList, Out0 <: HList] = ReversePrepend[P, S] { type Out = Out0 }
    implicit def hnilReversePrepend1[P <: HNil, S <: HList]: Aux[P, S, S] = ???
  }

  trait RuleX
  class Rule[-I <: HList, +O <: HList] extends RuleX
  type RuleN[L <: HList] = Rule[HNil, L]
  type Rule0 = RuleN[HNil]

  sealed trait RunResult[T] {
    type Out <: RuleX
  }

  object RunResult {
    implicit def fromAux[T, Out0 <: RuleX](implicit aux: Aux[T, Out0]): RunResult[T] { type Out = Out0 } = ???
    sealed trait Aux[T, Out]
    object Aux extends Aux2 {
      implicit def forFHList[I <: HList, R, In0 <: HList, Out0 <: HList](implicit x: Join.Aux[I, HNil, HNil, R, HNil, In0, Out0]): Aux[I ⇒ R, Rule[In0, Out0]] = ???
    }
    class Aux2 {
      implicit def forAny[T]: Aux[T, Rule0] = ???
    }
  }

  sealed trait Join[I <: HList, L1 <: HList, L2 <: HList, R] {
    type In <: HList
    type Out <: HList
  }

  object Join {
    implicit def join[I <: HList, L1 <: HList, L2 <: HList, R, In0 <: HList, Out0 <: HList]
    (implicit x: Aux[I, L1, L2, R, HNil, In0, Out0]): Join[I, L1, L2, R] { type In = In0; type Out = Out0 } = ???

    sealed trait Aux[I <: HList, L1 <: HList, L2 <: HList, R, Acc <: HList, In <: HList, Out <: HList]
    object Aux extends Aux1 {
      // if R == Unit convert to HNil
      implicit def forUnit[I <: HList, L1 <: HList, L2 <: HList, Acc <: HList, Out <: HList]
      (implicit x: Aux[I, L1, L2, HNil, Acc, I, Out]): Aux[I, L1, L2, Unit, Acc, I, Out] = ???

      // if R <: HList and L1 and L2 empty set Out = reversePrepend Acc before R
      implicit def terminate[I <: HList, R <: HList, Acc <: HList, Out <: HList](implicit x: ReversePrepend.Aux[Acc, R, Out]): Aux[I, HNil, HNil, R, Acc, I, Out] = ???
    }

    class Aux1 {
      // convert R to R :: HNil
      implicit def forAny[I <: HList, L1 <: HList, L2 <: HList, R, Acc <: HList, Out <: HList](implicit x: Aux[I, L1, L2, R :: HNil, Acc, I, Out]): Aux[I, L1, L2, R, Acc, I, Out] = ???
    }
  }
}

