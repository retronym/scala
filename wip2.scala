import scala.reflect.ClassTag

class Foo2[T] {
  def tabulate[T: ClassTag](n: Int)(f: Int => T): Array[T] =  ???
  def tabulate[T: ClassTag](n1: Int, n2: Int)(f: (Int, Int) => T): Array[_ <: Array[_ <: T]] = tabulate(n1)(i1 => tabulate(n2)(f(i1, _)))
  // def tabulate[T: ClassTag](n1: Int, n2: Int, n3: Int)(f: (Int, Int, Int) => T): Array[Array[Array[T]]] = tabulate(n1, n2)((i1, i2) => tabulate(n3)(f(i1, i2, _)))
}


class Foo3[T] {
  def tabulate[T: ClassTag](n: Int)(f: Int => T): List[T] =  ???
  def tabulate[T: ClassTag](n1: Int, n2: Int)(f: (Int, Int) => T): List[List[T]] = tabulate(n1)(i1 => tabulate(n2)(f(i1, _)))
  // def tabulate[T: ClassTag](n1: Int, n2: Int, n3: Int)(f: (Int, Int, Int) => T): Array[Array[Array[T]]] = tabulate(n1, n2)((i1, i2) => tabulate(n3)(f(i1, i2, _)))
}
