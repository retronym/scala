object Test extends App {
  def test(f: () => Int)(g: => Int): Unit = {
    val gFunc = () => g
    val isSameClosureClass = gFunc.getClass == f.getClass
    val isSame = gFunc eq f
    println("Is same closure class: "+isSameClosureClass+" is same closure: "+isSame)
  }

  // a local: evaluating a field at the call site can be observed, see byname-function0-elision.scala
  locally {
    val f = () => 5
    test(f)(f())
  }
}
