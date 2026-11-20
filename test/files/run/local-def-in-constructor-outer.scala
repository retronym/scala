//> using options -Wconf:cat=deprecation:s
// A local method in a constructor that refers to a member of an enclosing class, either the field
// for a captured variable or a lifted method, needs the outer parameter of the constructor.
class Par(val v: Int)

// capture through an enclosing local class
class P0 { def run(p: Int): Int = { class LocM { def mth(): Int = { class LocP extends Par({ def g = p; g }); new LocP().v } }; new LocM().mth() } }
// through an anonymous class
class P1 { def run(p: Int): Int = { val o = new Function0[Int] { def apply(): Int = { class LocP extends Par({ def g = p + 1; g }); new LocP().v } }; o() } }
// call of a local method lifted to the enclosing class
class P2 { def run(p: Int): Int = { class LocM { def mth(): Int = { def h() = p + 2; class LocP extends Par({ def g = h(); g }); new LocP().v } }; new LocM().mth() } }
// ... to a class two levels out
class P3 { def run(p: Int): Int = { def h() = p + 3; class LocM { def mth(): Int = { class LocP extends Par({ def g = h(); g }); new LocP().v } }; new LocM().mth() } }
// nested local methods
class P4 { def run(p: Int): Int = { class LocM { def mth(): Int = { class LocP extends Par({ def g = { def g2 = p + 4; g2 }; g }); new LocP().v } }; new LocM().mth() } }
// self-constructor call
class P5 { def run(p: Int): Int = { class LocM { def mth(): Int = { class LocP(val v: Int) { def this() = this({ def g = p + 5; g }) }; new LocP().v } }; new LocM().mth() } }
// statement after the self-constructor call
class P6 { def run(p: Int): Int = { class LocM { def mth(): Int = { class LocP(var v: Int) { def this() = { this(0); def g = p + 6; v = g } }; new LocP().v } }; new LocM().mth() } }
// a member class as the enclosing class, lifted method that uses its `this`
class P7 { class M { val m = 7; def mth(p: Int): Int = { def h() = p + m; class LocP extends Par({ def g = h(); g }); new LocP().v } } }
// captured variable
class P8 { def run(p: Int): Int = { var x = p; class LocM { def mth(): Int = { class LocP extends Par({ def g = { x += 8; x }; g }); new LocP().v } }; new LocM().mth() } }
// early definition
class P9 { def run(p: Int): Int = { class LocM { def mth(): Int = { class LocP extends { val e = { def g = p + 9; g } } with Par(e); new LocP().v } }; new LocM().mth() } }
// local methods calling each other
class P10 { def run(p: Int): Int = { class LocM { def mth(): Int = { class LocP extends Par({ def g1 = p + 10; def g2 = g1; g2 }); new LocP().v } }; new LocM().mth() } }

object Test {
  def main(args: Array[String]): Unit = {
    assert(new P0().run(10) == 10)
    assert(new P1().run(10) == 11)
    assert(new P2().run(10) == 12)
    assert(new P3().run(10) == 13)
    assert(new P4().run(10) == 14)
    assert(new P5().run(10) == 15)
    assert(new P6().run(10) == 16)
    val p7 = new P7
    assert(new p7.M().mth(10) == 17)
    assert(new P8().run(10) == 18)
    assert(new P9().run(10) == 19)
    assert(new P10().run(10) == 20)
  }
}
