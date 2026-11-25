//> using options -Xmixin-force-forwarders:false

trait T { def m = "T" }
class A { private def m = "A-private"; def callM = m }
class C extends A with T    // A.m is private, so C's only member m is T.m
class D extends JS with T   // JS.m is static, so D's only member m is T.m

class B
object B { def m = "B-object" }
class E extends B with T    // B has a static forwarder to B.m
class F
object F extends F with T   // F has a static forwarder to F.m, which is T.m

object Test {
  def t(f: => Any): Unit = try println(f) catch { case e: Throwable => println(e.getClass.getName) }
  def main(args: Array[String]): Unit = {
    t((new C).m)
    t((new C: T).m)
    t((new C).callM)
    t((new D).m)
    t((new D: T).m)
    t((new E).m)
    t(B.m)
    t(F.m)
  }
}
