//> using options -Ybreak-cycles
//
trait C[T] {}

abstract class A {
 type T >: C[T] <: C[C[T]]
}
