class V(val s: String) extends AnyVal { override def toString = "V(" + s + ")" }

trait R[A] {
  var last: Option[A] = None
  def g(a: A): String = { last = Some(a); "R.g(" + a + ")" }
  def h(): A = last.get
}
// super call in a trait: goes through the super accessor T$$super$g
trait T extends R[V] {
  override def g(a: V): String = "T.g(" + super.g(a) + ")"
  override def h(): V = new V("T.h(" + super.h().s + ")")
}
class C extends T
// super call in a class: direct invokespecial, for comparison
class K extends R[V] {
  override def g(a: V): String = "K.g(" + super.g(a) + ")"
  override def h(): V = new V("K.h(" + super.h().s + ")")
}
// super call in a class from inside a closure: goes through a super accessor again
class L extends R[V] {
  override def g(a: V): String = "L.g(" + (() => super.g(a))() + ")"
  override def h(): V = new V("L.h(" + (() => super.h())().s + ")")
}

// the accessor is generic, the super target it is bound to in the class takes and returns the value class
trait S extends R[V] {
  override def g(a: V): String = "S.g(" + a + ")"
  override def h(): V = new V("S.h")
}
trait U[B] extends R[B] {
  abstract override def g(a: B): String = "U.g(" + super.g(a) + ")"
  abstract override def h(): B = super.h()
}
class M extends S with U[V]

object Test {
  def t(f: => Any): Unit = try println(f) catch { case e: Throwable => println(e.getClass.getName) }
  def main(args: Array[String]): Unit = {
    val v = new V("v")
    val k = new K; t(k.g(v)); t(k.last.get.s); k.last = Some(v); t(k.h())
    val c = new C; t(c.g(v)); t(c.last.get.s); c.last = Some(v); t(c.h())
    val l = new L; t(l.g(v)); t(l.last.get.s); l.last = Some(v); t(l.h())
    val m = new M; t(m.g(v)); t(m.h())
  }
}
