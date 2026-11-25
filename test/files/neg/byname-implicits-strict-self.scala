class T(val p: T) { override def toString = "T(" + p + ")" }
object T { implicit def d(implicit p: T): T = new T(p) }

class U(val v: V)
class V(val u: U)
object U { implicit def u(implicit v: V): U = new U(v) }
object V { implicit def v(implicit u: U): V = new V(u) }

object Test {
  def f(implicit t: => T): T = t
  def g(implicit u: => U): U = u
  // the same searches from a by-value parameter
  def h(implicit t: T): T = t

  f
  g
  h
}
