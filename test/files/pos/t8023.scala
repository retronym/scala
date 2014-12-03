import language._


// object Test {
//   def foo = (null: Any) match {
//     case a: A[k] =>
//       // error: kinds of the type arguments (k) do not conform to the 
//       // expected kinds of the type parameters (type K) in class B.
//       new B[k]()
//       // ()
//   }
// }

// class A[K[L[_]]]

// class B[K[M[_]]]


// object Test2 {
//   def foo = (null: Any) match {
//     case a: A[k] => new B[k]() // this one worked before as the info of `A` was complete
//     // ()
//   }
// }


class C[K]
class D[K]

object Test3 {
  def foo = (null: Any) match {
    case a: C[k] => new C[k]() // this one worked before as the info of `A` was complete
    // ()
  }
}
