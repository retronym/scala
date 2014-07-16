trait S[A]

trait Test {
  // def test = {
  //   val res: S[AA with String] forSome { type AA } = null
  //   var temp: S[AA with String] forSome { type AA }  = _
  //   temp = res // error
  //   temp
  // }
 

  def test2 = {
    val res: S[AA] forSome { type AA } = null
    val temp: S[AA] forSome { type AA } = res
    temp
  }

  def test3 = {
    val res: S[AA with String] forSome { type AA } = null

    // type error. subtyping can't unify existentials within
    // refinement types.
    //
    // We end up with:
    //
    // (?AA with String) <:< (AA&0 with String)
    //   |                      `-- skolem based on the type AA in `res`
    //   \------------------------- type variable based on AA in the type of `temp`.
    //  (?AA with String) <:< AA&0
    //
    //  This gets to:
    //
    //    def retry(lhs: Type, rhs: Type)   = isSubType(lhs, rhs, depth)
    //    def abstractTypeOnRight(lo: Type) = isDifferentTypeConstructor(tp2, lo) && retry(tp1, lo)
    //    ...
    //      case _: TypeSymbol if sym2.isDeferred => abstractTypeOnRight(tp2.bounds.lo) || fourthTry
    //
    //  Which registers Nothing as an upper constraint for the type varaible, which is not satisfiable.
    val temp: S[AA with String] forSome { type AA } = res
    temp
  }

  def test4 = {
    type X[A] = A with String
    val res: S[X[AA]] forSome { type AA } = null
    val temp: S[X[AA]] forSome { type AA } = res
    temp
  }
}
