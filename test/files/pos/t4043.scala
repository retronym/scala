object TypeParam {
  trait GC[K[_ <: H0], H0]
 
  trait PA[H1] {
    type Apply[A <: H1] = Any
  }
 
  // error:
  // kinds of the type arguments ([A <: H1]Any,Int) do not conform to the expected kinds of the type parameters (type K,type H0) in trait GC.
  // [A <: H1]Any's type parameters do not match type K's expected parameters:
  // type A's bounds >: Nothing <: H1 are stricter than type _'s declared bounds >: Nothing <: H0
  type a = GC[PA[Int]#Apply, Int]
}
