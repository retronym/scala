import scala.language.higherKinds
 
trait TypePreservingFn[TPT[X[XX <: TPT[X]]]]
 
trait Validator[ValT, This[_ <: Validator[ValT,This]]]
 
trait Foo {
  type A
  type V[This[XX <: Validator[A, This]]] <: Validator[A, This]
  type X = TypePreservingFn[V] // okay
}
 
class Bar extends Foo {
  type Y = TypePreservingFn[V] // nok 
}
