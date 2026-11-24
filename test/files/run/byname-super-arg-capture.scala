// A class in a by-name super-constructor argument may capture a constructor
// parameter or a local value, like the `() => ...` form of the argument does.

class P(bn: => Any) { def get = bn.asInstanceOf[Runnable] }
class Q(f: () => Any) { def get = f().asInstanceOf[Runnable] }
class V(v: Any) { def get = v.asInstanceOf[Runnable] }

class By(x: Int) extends P(new Runnable { def run() = println(s"By $x") })
class ByL(x: Int) extends P({ class L extends Runnable { def run() = println(s"ByL $x") }; new L })
class ByNested(x: Int) extends P(new Runnable { def run() = new Runnable { def run() = println(s"ByNested $x") }.run() })
class Fn(x: Int) extends Q(() => new Runnable { def run() = println(s"Fn $x") })
class Strict(x: Int) extends V(new Runnable { def run() = println(s"Strict $x") })

class Outer(val name: String) {
  class In(x: Int) extends P(new Runnable { def run() = println(s"In $x $name") })
}

object Test {
  def local(y: Int) = {
    class K extends P(new Runnable { def run() = println(s"K $y") })
    new K
  }
  def localStrict(y: Int) = {
    class KS extends V(new Runnable { def run() = println(s"KS $y") })
    new KS
  }
  def main(args: Array[String]): Unit = {
    new By(1).get.run()
    new ByL(2).get.run()
    new ByNested(3).get.run()
    new Fn(4).get.run()
    new Strict(5).get.run()
    val o = new Outer("o")
    new o.In(6).get.run()
    local(7).get.run()
    localStrict(8).get.run()
  }
}
