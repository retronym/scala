//> using options -Wconf:cat=other-nullary-override:s

class A {
  def f[T]: List[T] = Nil
  def g[T](): List[T] = Nil
  def callF = f[Int]
  def callG = g[Int]()
}
class B extends A {
  override def f[T](): List[T] = List(null.asInstanceOf[T])
  override def g[T]: List[T] = List(null.asInstanceOf[T])
}

trait X { def m[T]: Option[T] }
trait Y { def m[T](): Option[T] = None }
class Z extends X with Y

object Test {
  def methods(c: Class[_]) =
    c.getDeclaredMethods.toList.filter(m => Set("f", "g", "m")(m.getName)).map(m => s"${m.getName}/${m.getParameterCount}${if (m.isBridge) " bridge" else ""}").sorted

  def main(args: Array[String]): Unit = {
    val b = new B
    val a: A = b
    // the overriding member is selected whichever way the member is defined in the static type
    println((a.f[Int].size, a.g[Int]().size, b.f[Int]().size, b.g[Int]().size, a.callF.size, a.callG.size))
    println((new A).f[Int].size)
    val z = new Z
    println((z.m[Int](), (z: X).m[Int], (z: Y).m[Int]()))
    // one method for each member, and no bridges
    println(methods(classOf[A]))
    println(methods(classOf[B]))
    println(methods(classOf[Z]))
  }
}
