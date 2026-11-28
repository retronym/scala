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

// A member of `Outers` and a local class or trait of the same name that extends it with another prefix: both need
// an outer accessor relative to the enclosing class.
class Outers(val name: String) {
  trait T { def member = name }
  class C { def member = name }

  def localTrait(other: Outers): Unit = {
    trait T extends other.T { def local = Outers.this.name }
    val t = new T {}
    println(t.member)
    println(t.local)
  }

  def localSubclass(other: Outers): Unit = {
    class C extends other.C { def local = Outers.this.name }
    val c = new C
    println(c.member)
    println(c.local)
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

  // same-named local traits split between a superclass and a subclass; they also capture local values
  def splitAcrossSuperclass(): Unit = {
    class Base { def f = "base" }
    val a = "outer"
    trait T extends Base {
      private val v = a
      private var w = a
      private lazy val x = a
      private def y = a
      class I { def get = y }
      def go = List(v, w, x, new I().get).mkString(" ")
      abstract override def f = "outer " + super.f
    }
    class B extends Base with T
    locally {
      val b = "inner"
      trait T extends Base {
        private val v = b
        private var w = b
        private lazy val x = b
        private def y = b
        class J { def get = y }
        def gi = List(v, w, x, new J().get).mkString(" ")
        abstract override def f = "inner " + super.f
      }
      class C extends B with T
      val c = new C
      println(c.go)
      println(c.gi)
      println(c.f)
    }
  }

  // the enclosing classes of the two traits are same-named local classes
  def sameNamedEnclosingClasses(): Unit = {
    class A(val s: String) { trait T { def go = s } }
    val a1 = new A("first")
    locally {
      class A(val s2: String) {
        trait T { def gi = s2 }
        class C extends a1.T with T
      }
      val a2 = new A("second")
      val c = new a2.C
      println(c.go)
      println(c.gi)
    }
  }

  // `private[T]` members are expanded relative to `T`
  def qualifiedPrivate(): Unit = {
    trait T { private[T] val x = "outer"; private[T] def y = "outer"; def go = x + " " + y }
    class B extends T
    locally {
      trait T { private[T] val x = "inner"; private[T] def y = "inner"; def gi = x + " " + y }
      class C extends B with T
      val c = new C
      println(c.go)
      println(c.gi)
    }
  }

  // the setter for a captured value and the private member must keep their names across lambdalift
  def captureAndPrivate(): Unit = {
    val a = "captured"
    trait T { private var x = "private"; def go = a + " " + x; class I { def get = a + " " + x } }
    val t = new T {}
    println(t.go)
    println(new t.I().get)
  }

  def main(args: Array[String]): Unit = {
    lazyValAndVar()
    methodCalledFromNestedClass()
    Lib.make()
    splitAcrossSuperclass()
    sameNamedEnclosingClasses()
    qualifiedPrivate()
    captureAndPrivate()
    val here = new Outers("this")
    here.localTrait(new Outers("other"))
    here.localSubclass(new Outers("other"))
  }
}
