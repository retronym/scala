import j.JP_1

// The JVM permits access to a protected static of `JP_1` from a subclass,
// but not from classes nested in it; those need an accessor in `C`.
class C extends JP_1[String] {
  def direct = JP_1.sm(0)
  class Inner {
    def a = JP_1.sm(1)
    def b = { JP_1.sf += 1; JP_1.sf }
    def c = JP_1.sz() + JP_1.sg("g") + JP_1.sv("a", "b")
  }
  object O { def a = JP_1.sm(2) }
  def anon = new Runnable { def run() = println(JP_1.sm(3)) }
  def byName = Option(4).map(i => JP_1.sm(i)).get
}
