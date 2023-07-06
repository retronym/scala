class Test {
  implicit def conv(rde: ReferenceDataEnum[_]): TypedEnum[_, _ <: ResultKey[_, _ <: AnyRef]] = null
  def foo(rde: ReferenceDataEnum[_]) = {
    J.m(rde)
    J.m(rde.asTypedEnum)
//    J.m(rde.asRawTypedEnum)
    ()
  }
}
