trait Marker[X]

class C[A] {
  def withB[B](b: B): Marker[A with B] = ???
}

class Test {
  class T2
  def test[T1]: Unit = {
    val c = new C[T1]
    val res: Marker[T1 with T2] = c.withB(new T2)
  }
}
