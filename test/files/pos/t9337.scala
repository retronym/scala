import scala.language.higherKinds
 
trait TypePreservingFn[TPT[X <: TPT[X]]]
 
trait Validator[ValT, This <: Validator[ValT,This]]
 
trait Foo {
  type A
  type V[This <: Validator[A, This]] <: Validator[A, This]
  // type X = TypePreservingFn[V] // okay
}
 
class Bar extends Foo {
  type Y = TypePreservingFn[V] // nok 
}
