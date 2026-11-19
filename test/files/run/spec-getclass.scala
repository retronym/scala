// `t.getClass` in a specialized method must return the class of the box, as in the generic method,
// not the primitive class.

class Holder[@specialized(Int, Double) T](val t: T) {
  def cls: Class[_ <: T] = t.getClass
  def name: String = t.getClass.getName
}

object Test {
  def cls[@specialized(Int) T](t: T): Class[_] = t.getClass
  def bounded[@specialized(Int, Long) T](t: T): Class[_ <: T] = t.getClass
  def all[@specialized(Int, Boolean, Unit) T](t: T): String = t.getClass.getSimpleName
  def ref[@specialized(Int) T](t: T, s: String): Class[_] = s.getClass

  def main(args: Array[String]): Unit = {
    println(List(cls(1), cls("s"), bounded(1), bounded(2L), bounded("b")))
    println(List(all(1), all(true), all(()), all("s")))
    println(List(new Holder(1).cls, new Holder(1.5).name, new Holder("h").cls, ref(1, "r")))
  }
}
