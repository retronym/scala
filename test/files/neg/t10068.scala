//> using options -Xelide-below WARNING -Xsource:2.13
//
import annotation._, elidable._

abstract class C {
  @elidable(INFO) val i: Int = 42
  @elidable(INFO) lazy val j: Int = 42
  @elidable(INFO) var k: Int = 42
  @elidable(INFO) def f: Int
}
@elidable(INFO) class D

class E @elidable(INFO) () { val z = 5 }
class F {
  @elidable(INFO) def this(x: Int) = this()
}

// not elided at this level, so not an error
class G @elidable(SEVERE) () { val z = 5 }
class H {
  @elidable(SEVERE) def this(x: Int) = this()
}
