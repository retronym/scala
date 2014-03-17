trait T[A] {
  type XX = A
  type X = Int
}

class C[X <: String] extends T[X] {
  def foo(x: X): String = x
  def bar(x: XX) = ()
  // was: error: type mismatch;
  // found   : C.this.X
  // required: String
}

// object T2 {
//   type X = String
//   class C extends T[Boolean] {
//     def foo(x: X): Int = x * x
//     def bar(x: XX): Any = ()
//   }
// }
