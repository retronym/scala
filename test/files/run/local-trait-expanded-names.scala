object Lib {
  trait Logger {
    private var count = 1
    private lazy val tag = "member"
    def member = s"$tag $count"
  }
  // a local trait with the name of a member trait of the enclosing object
  def make(): Unit = {
    trait Logger extends Lib.Logger {
      private var count = 2
      private lazy val tag = "local"
      def local = s"$tag $count"
    }
    val l = new Logger {}
    println(l.member)
    println(l.local)
  }
}

object Test {
  def lazyValAndVar(): Unit = {
    trait T { private lazy val x = "outer"; private var y = "outer"; def go = (() => x + " " + y)() }
    type OT = T
    def inner(): Unit = {
      trait T { private lazy val x = "inner"; private var y = "inner"; def gi = (() => x + " " + y)() }
      class C extends OT with T
      val c = new C
      println(c.go)
      println(c.gi)
    }
    inner()
  }

  // a private method that is called from a nested class is made public under its expanded name
  def methodCalledFromNestedClass(): Unit = {
    trait T { private def x = "outer"; class I { def get = x }; def go = new I().get }
    type OT = T
    def inner(): Unit = {
      trait T { private def x = "inner"; class J { def get = x }; def gi = new J().get }
      class C extends OT with T
      val c = new C
      println(c.go)
      println(c.gi)
    }
    inner()
  }

  def main(args: Array[String]): Unit = {
    lazyValAndVar()
    methodCalledFromNestedClass()
    Lib.make()
  }
}
