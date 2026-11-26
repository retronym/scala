// A specialized type parameter that occurs in a method only under a by-name parameter.

abstract class AC[@specialized(Long) A] {
  def m(p: Int, q: => A): Int = p
}
class C extends AC[Long] {
  override def m(p: Int, q: => Long): Int = p + 1
}

trait M[@specialized(Int) K, @specialized(Int) V] {
  def getOrElseUpdate(k: K, default: => V): Unit
}
class IntM extends M[Int, Int] {
  def getOrElseUpdate(k: Int, default: => Int): Unit = println(k + default)
}

trait AC2[@specialized(Int) A, @specialized(Int) B] {
  def m(q: => B): A
}
class D extends AC2[Int, Int] {
  def m(q: => Int): Int = q + 1
}

object Test {
  def main(args: Array[String]): Unit = {
    println(new C().m(1, 2L))
    println((new C(): AC[Long]).m(1, 2L))
    new IntM().getOrElseUpdate(1, 2)
    (new IntM(): M[Int, Int]).getOrElseUpdate(1, 2)
    println(new D().m(1))
    println((new D(): AC2[Int, Int]).m(1))
  }
}
