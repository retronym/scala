//> using options -Xmixin-force-forwarders:false

trait A { def m = "A" }
trait B extends A { def m: String }   // abstract redeclaration; A.m stays the implementation
class E extends B
class X extends A
class F extends X with B

trait G[T] { def g(t: T): String = "G" }
trait H extends G[String] { def g(t: String): String }
class I extends H

object Test {
  def t(f: => Any): Unit = try println(f) catch { case e: Throwable => println(e.getClass.getName) }
  def main(a: Array[String]): Unit = {
    t((new E).m)
    t((new E: A).m)
    t((new E: B).m)
    t((new F).m)
    t((new F: B).m)
    t((new I).g(""))
    t((new I: G[String]).g(""))
  }
}
