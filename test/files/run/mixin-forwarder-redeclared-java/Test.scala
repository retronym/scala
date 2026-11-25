// T redeclares the Java default method m as abstract. A concrete member beats a deferred one,
// so the member m of C and D is J.m, and they are concrete classes.
trait T extends J { def m: String }
class C extends JC with T
class D extends T with J
class SC extends JC
class C2 extends SC with T

object Test {
  def t(f: => Any): Unit = try println(f) catch { case e: Throwable => println(e.getClass.getName) }
  def main(a: Array[String]): Unit = {
    t((new C).m)
    t((new C: J).m)
    t((new C: T).m)
    t((new D).m)
    t((new D: J).m)
    t((new C2: T).m)
  }
}
