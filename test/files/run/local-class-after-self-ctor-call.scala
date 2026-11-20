// Local classes, objects and traits defined after the self-constructor call
// of a secondary constructor may refer to the instance under construction.

class C(val v: Int) {
  var r: List[Any] = Nil
  class Inner { def iv = v * 10 }
  def this() = {
    this(1)
    class L { def vv = v }
    object M { def vv = v + 1 }
    trait T { def tv = v + 2 }
    class TC extends T
    case class CC(x: Int) { def cv = v + x }
    class LI extends Inner
    r = List(new L().vv, M.vv, new TC().tv, new T {}.tv, CC(3).cv, CC.apply(4).cv, new LI().iv)
  }
}

class O(val ov: Int) {
  class D(val v: Int) {
    var r: List[Any] = Nil
    def this() = {
      this(5)
      class L { def vv = v + ov }
      object M { def vv = v * ov }
      case class CC(x: Int) { def cv = v + ov + x }
      r = List(new L().vv, M.vv, CC(1).cv)
    }
    // A class in the self-constructor call itself cannot refer to the instance,
    // its outer is the outer class.
    def this(s: String) = {
      this({ class K { def k = ov + s.length }; new K().k })
      r = List(s)
    }
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    println(new C().r)
    val o = new O(100)
    println(new o.D().r)
    val d = new o.D("abc")
    println((d.v, d.r))
  }
}
