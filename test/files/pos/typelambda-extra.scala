class C {
  def foo[M[A]]: M[Int] = ???

  type A[x] = (String, x)
  val a: (String, Int) = foo[[x] => (String, x)]


  class C { type A = String; type M = ([_ <: this.A] => Int)[Nothing]; }
}
