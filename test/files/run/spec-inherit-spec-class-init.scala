//> using options -nowarn
// A specialized class that extends another specialized class inherits the generic parent, not its
// specialized variant. The parent's initialisation must still run in a specialized instance of the child.

class Parent[@specialized(Int) T](x: T) { val a: T = x; println("Parent init, a = " + a) }
class Child[@specialized(Int) T](x: T) extends Parent[T](x) { val e: T = x; println("Child init, e = " + e) }
class GrandChild[@specialized(Int) T](x: T) extends Child[T](x) { val g: T = x; println("GrandChild init, g = " + g) }
class NoFields[@specialized(Int) T](x: T) extends Parent[T](x) { println("NoFields init"); def f(t: T) = t }
class Child2[@specialized(Int) T](x: T) extends Parent[Int](1) { val e: T = x; println("Child2 init, e = " + e) }
class Unguarded[@specialized(Int) T] { def f(t: T) = t }
class Child3[@specialized(Int) T](x: T) extends Unguarded[Int] { val e: T = x; println("Child3 init, e = " + e) }

object Test {
  def main(args: Array[String]): Unit = {
    val c = new Child(4); println((c.a, c.e))
    val s = new Child("s"); println((s.a, s.e))
    val g = new GrandChild(5); println((g.a, g.e, g.g))
    val n = new NoFields(6); println(n.a)
    val c2 = new Child2(7); println((c2.a, c2.e))
    val c3 = new Child2("t"); println((c3.a, c3.e))
    val p = new Parent(8); println(p.a)
    val c4 = new Child3(9); println(c4.e)
    val c5 = new Child3("u"); println(c5.e)
  }
}
