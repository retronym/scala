import annotation._

class C {
  def foo = {
    class Parent {
      @tailrec def bar: Int = {println("here we go again"); bar}
    }
    class Child extends Parent {
      def notBar = 42
    }
  }
}
