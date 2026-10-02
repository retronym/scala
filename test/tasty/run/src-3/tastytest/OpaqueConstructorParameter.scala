package tastytest

object OpaqueConstructorParameter {
  opaque type Id[X] <: X = X
  opaque type Id2[X] = X
  opaque type Arr[X] = Array[X]

  def id[X](x: X): Id[X] = x
  def id2[X](x: X): Id2[X] = x
  def arr[X](x: Array[X]): Arr[X] = x

  object Api {
    def unbounded(x: Id2[Int]): Int = 1
    def array(x: Arr[Int]): Int = 2
    def nested(x: Id[Id[Long]]): Int = 3
    def arrayOfId(x: Array[Id[Int]]): Int = 4
    def arrayOfId2(x: Array[Id2[Int]]): Int = 5
    def result: Id[Int] = id(6)
  }

  trait AbstractHK {
    type F[X] <: X
    def f(x: F[Int]): Int
  }

  trait Overridable {
    def m(x: Id[Long]): Int
  }
}

class OpaqueHolder(val longId: OpaqueConstructorParameter.Id[Long], val stringId: OpaqueConstructorParameter.Id[String])
