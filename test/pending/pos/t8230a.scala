trait Arr[T1]
object Arr {
  def apply[T2](xs: T2): Arr[T2]    = null
  def apply(x: Long) : Arr[Long] = null
}

object I {
  implicit def arrToTrav[T3] (a: Arr[T3]) : Traversable[T3]    = null
  implicit def longArrToTrav(a: Arr[Long]): Traversable[Long] = null
}
  
object Test {
  import I.arrToTrav
  val x: Traversable[Any] = Arr.apply("2") // fails
  //
  // exprTypeArgs(tparams = [T2], tvars = [?T2])
  //   resTpe = Arr[T2]
  //   resTpeInst = Arr[?T2]
  //   isWeaklyCompatible(tp = Arr[?T2], pt = Traversable[Any])
  //     viewExists(Arr[?T2], Traversable[Any])
  //       undoLog
  //          typedImplicit(info = arrToTrav)
  //             methTypeArgs(tvars = [?T2], argTpes = [Arr[?T2]])
  //                restpeInst = Traversable[?T3]
  //                isConservativelyCompatible(restpeInst, pt = Traversable[Any]
  //                isCompatible(Arr[?T2], Arr[T3])
  //                  ?T2 =:= ?T3
  //                solvedTypes(tparams = [T3], tvars = [?T3])
  //                   solve
  //                   tvars map instantiate
  //                      instantiate.apply(?T3)
  //                         instantiate.apply(?T3.const.inst (= ?T2))
  //                           instantiate.apply(?T3.const.inst (= ?T2))
  //                             (?T2).constr.instValid = false
  //                             throw "no unique instantiation of type variable"
  //   ...
  //   solve(tparams = T2, tvars = ?T2)


  // Explicitly writing the view is okay, as we typecheck the argument first and solve for ?T2.
  arrToTrav(Arr.apply("2"))
}
