trait OuterA[Inner_] { type Inner = Inner_ }
trait OuterB         { type Inner }

abstract class Test4 {
  def test = {
    val o: OuterA[_] = ???
    def foo(x: Any): o.Inner = ???
    val etaExpanded: Any => o.Inner = foo
  }
}
