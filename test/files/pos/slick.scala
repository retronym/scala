trait OuterA[Inner_] { type Inner = Inner_ }
trait OuterB         { type Inner }

//abstract class Test1 {
//  val o: OuterA[_]
//  def foo(a: Any): o.Inner
//  val etaExpanded: Any => o.Inner = foo // used to be disallowed, now disallowed (found: Any => Any, required: ...)
//}
//
//abstract class Test2 {
//  def test(o: OuterA[_]) {
//    def foo(a: Any): o.Inner = ???
//    val etaExpanded: Any => o.Inner = foo // used to be allowed, now disallowed (found: Any => Any, required ...)
//  }
//}
//
//abstract class Test3 {
//  def test(o: OuterB) {
//    def foo(a: Any): o.Inner = ???
//    val etaExpanded: Any => o.Inner = foo // used to be disallowed, now disallowed (method with dependent type (a: Any)o.Inner cannot be converted to function value)
//  }
//}

abstract class Test4 {
  def test = {
    val o: OuterA[_] = ???
    def foo(x: Any): o.Inner = ???
    val etaExpanded: Any => o.Inner = foo // used to be disallowed, now disallowed (found: Any => Any, required: ...)
  }
}
