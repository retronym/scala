package scalapkg

import javapkg.JavaClass

object Main extends App {
  val someScala = new SomeScala("abc")
  new JavaClass().useInner0(new someScala.InnerClass)
  new JavaClass().useInner1(new someScala.InnerClass)
}

class SomeScala(str: String) {
  class InnerClass {
    def printStr() = println(str)
  }
}
