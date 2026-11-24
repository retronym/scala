// A type pattern `C#X` must check that the outer instance of the scrutinee is a `C` (SLS 8.2).
class A {
  class X { def outer: A.this.type = A.this }
}
class B extends A
class C extends A
trait M
class G[T] extends A

object Test {
  def cx(x: A#X): String = x match { case _: C#X => "C#X"; case _ => "other" }
  def ax(x: Any): String = x match { case _: A#X => "A#X"; case _ => "other" }
  def cmx(x: A#X): String = x match { case _: (C with M)#X => "(C with M)#X"; case _ => "other" }
  def gx(x: A#X): String = x match { case _: G[_]#X => "G[_]#X"; case _ => "other" }
  def tx[T <: A](x: A#X): String = x match { case _: T#X => "T#X"; case _ => "other" }
  def outer(x: A#X): String = x match { case cx: C#X => val c: C = cx.outer; if (c ne null) "C#X" else "null"; case _ => "other" }
  def seq(xs: List[A#X]): String = xs match { case List(_: C#X, _: B#X) => "C#X, B#X"; case _ => "other" }

  def main(args: Array[String]): Unit = {
    val b = new B
    val c = new C
    val cm = new C with M
    val g = new G[String]
    val bx: A#X = new b.X
    println(cx(bx))
    println(cx(new c.X))
    println(cx(new cm.X))
    println(bx.isInstanceOf[C#X]) // isInstanceOf does not look at the outer instance (SLS 12.1)
    println(ax(bx))
    println(cmx(new c.X))
    println(cmx(new cm.X))
    println(gx(bx))
    println(gx(new g.X))
    println(tx[C](bx)) // T erases to A
    println(outer(bx))
    println(outer(new c.X))
    println(seq(List(new c.X, new b.X)))
    println(seq(List(new b.X, new c.X)))
  }
}
