// FX1 and FX2 are linked to specialization only by the method type parameter of P.a,
// which is read from P's pickle.
class FX extends P {
  override def a[@specialized(Int) T](t: T): List[T] = Nil
}
class FX1 extends P {
  override def a[@specialized(Double) T](t: T): List[T] = Nil
}
class FX2 extends P {
  override def a[T](t: T): List[T] = Nil
}
