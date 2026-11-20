import scala.language.reflectiveCalls

object Test {
  def main(args: Array[String]): Unit = {
    assert(t1 == "hi")
    assert(t2 == 1)
    t3()
    t4()
  }

  def t1: Object = {
    val f: { def apply(s: String): Object } = (x: String) => x
    f("hi")
  }

  def t2: Int = {
    def byName(b: => Int): Int = b
    def namer[A, B](f: A => B): (A => B) { def apply(i: A): B } = f

    val namedFunction = namer(byName _)
    namedFunction(1)
  }

  // A parameter of primitive type in the refinement is `Object` in the method of the receiver if that is a lambda
  // for a specialized function or one with a boxing adapter, or if its class declares it with a type parameter.
  def t4(): Unit = {
    def as[T](t: T): T = t
    assert(as[{ def apply(x: Int): Int }]((x: Int) => x + 1).apply(3) == 4)
    assert(as[{ def apply(x: Char): Char }]((x: Char) => x).apply('c') == 'c')
    assert(as[{ def apply(x: Int): String }]((x: Int) => "s" + x).apply(1) == "s1")
    assert(as[{ def apply(x: String): Int }]((x: String) => x.length).apply("s") == 1)
    assert(as[{ def apply(): Int }](() => 1).apply() == 1)
    assert(as[{ def apply(x: Int, y: Int): Int }]((x: Int, y: Int) => x + y).apply(1, 2) == 3)
    assert(as[{ def apply(x: Int, y: String): Int }]((x: Int, y: String) => x + y.length).apply(1, "s") == 2)
    assert(as[{ def apply(x: Long, y: Double): Boolean }]((x: Long, y: Double) => x < y).apply(1L, 2d))
    assert(as[{ def apply(x: Boolean): Unit }]((x: Boolean) => assert(x)).apply(true) == ((): Any))

    val sam: Sam[Int] = _ + 1
    assert(as[{ def ap(x: Int): Int }](sam).ap(1) == 2)
    assert(as[{ def ap(x: Int): Int }](new Sam[Int] { def ap(x: Int) = x + 1 }).ap(1) == 2)
    assert(as[{ def ap(x: Int): Int }](new Gen[Int]).ap(1) == 1)

    // one call site, receivers that need either lookup
    def call(f: { def apply(x: Int): Int }): Int = f(1)
    val fs = List[Int => Int](_ + 1, new (Int => Int) { def apply(x: Int) = x + 1 }, _ + 1)
    assert(fs.map(call(_)) == List(2, 2, 2))

    // the method with the primitive parameter is preferred
    class Overloaded { def m(x: Any): String = "any"; def m(x: Int): String = "int" }
    assert(as[{ def m(x: Int): String }](new Overloaded).m(1) == "int")

    // the exception names the method of the refinement
    try {
      "".asInstanceOf[{ def m(x: Int, y: String): Int }].m(1, "")
      throw new Error()
    } catch {
      // JDK 8 separates the parameter types with ", ", later JDKs with ","
      case e: NoSuchMethodException => assert(e.getMessage.replace(" ", "") == "java.lang.String.m(int,java.lang.String)", e.getMessage)
    }
  }

  // Not sure how to fix this one.. https://github.com/scala/bug/issues/10334
  def t3(): Unit = {
    val f1 = new T[A] {
      def m(x: A) = "f1-a"
      def m(x: B) = "f1-b"
       // the m(Object)Object bridge method invokes (A)Object
    }

    val f2 = new T[B] {
      def m(x: A) = "f2-a"
      def m(x: B) = "f2-b"
       // the (Object)Object bridge method invokes (B)Object
    }

    val g1: T[C] = f1
    val g2: T[C] = f2

    assert(g1.m(new C) == "f1-a")
    assert(g2.m(new C) == "f2-b")

    val s1: { def m(s: C): Object } = g1
    val s2: { def m(s: C): Object } = g2

    // the reflective lookup doesn't find `m(C)Object`
    try {
      s1.m(new C) // should invoke `m(A)Object`
      throw new Error()
    } catch {
      case _: java.lang.NoSuchMethodException =>
    }

    // the reflective lookup doesn't find `m(C)Object`
    try {
      s2.m(new C) // should invoke `m(B)Object`
      throw new Error()
    } catch {
      case _: java.lang.NoSuchMethodException =>
    }
  }
}

class A
class B extends A
class C extends B

trait Sam[A] { def ap(a: A): A }
class Gen[A] { def ap(a: A): A = a }

trait T[-A] {
  def m(a: A): Object
}
