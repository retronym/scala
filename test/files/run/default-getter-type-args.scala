// Default getters in the same parameter list as inferred arguments receive
// the type arguments solved for the application, not `Nothing`.
import scala.collection.mutable.ListBuffer

class Box[T](val a: T, val b: T = null.asInstanceOf[T]) { def get: T = if (b == null) a else b }
class Box3[T](a: T, b: T = null.asInstanceOf[T])(c: T = a) { override def toString = "" + a + b + c }
case class CC[T](a: T, b: T = null.asInstanceOf[T])

// an override that renames the type parameters inherits the default getter `A.m$default$2[T]`
class A { def m[T](a: T, b: T = null.asInstanceOf[T]): T = if (b == null) a else b }
class B extends A { override def m[U](a: U, b: U): U = if (b == null) a else b }
// or swaps them: the getter's `T` is the override's `S`
class A2 { def m[S, T](a: T, b: T = null.asInstanceOf[T]): T = if (b == null) a else b }
class B2 extends A2 { override def m[T, S](a: S, b: S): S = if (b == null) a else b }
class Super extends A { override def m[U](a: U, b: U): U = super.m(a, b) }
class BoxR[T](val b: T = null.asInstanceOf[T], val a: T) { def get: T = if (b == null) a else b }
class Outer { class Box[T](val a: T, val b: T = null.asInstanceOf[T]) { def get: T = if (b == null) a else b } }

object Test {
  def pick[T](a: T, b: T = null.asInstanceOf[T]): T = if (b == null) a else b
  def pickL[T](a: T)(b: T = null.asInstanceOf[T]): T = if (b == null) a else b
  def pickR[T](b: T = null.asInstanceOf[T], a: T): T = if (b == null) a else b
  def pair[T](a: T, b: T = null.asInstanceOf[T]): (T, T) = (a, b)
  def three[T](a: T, b: T = null.asInstanceOf[T])(c: T = a): String = "" + a + b + c
  def add[T](a: T, buf: ListBuffer[T] = new ListBuffer[T]): ListBuffer[T] = buf += a
  // the default contributes to inference
  def fromDefault[T](x: T = 1): T = x
  def opt[T](a: T, b: Option[T] = None): String = "" + a + b
  def byName[T](a: T, b: => T = null.asInstanceOf[T]): T = if (b == null) a else b
  def imp[T](a: T, b: T = null.asInstanceOf[T])(implicit ord: Ordering[T]): T = if (b == null) a else ord.max(a, b)
  def impD[T](a: T, b: T = null.asInstanceOf[T])(implicit d: DummyImplicit): T = if (b == null) a else b
  // the type parameter is solved only by the second argument list
  def later[T](a: Int, b: T = null.asInstanceOf[T])(c: T): String = "" + a + b + c
  def laterP[T](a: Int, b: T = null.asInstanceOf[T])(c: T): T = if (b == null) c else b

  def main(args: Array[String]): Unit = {
    println(pick("a"))
    println(pick[String]("a"))
    println(pick(a = "a"))
    println(pickL("a")())
    println(pair("a"))
    println(three("s")())
    println(three(1)())
    println(add(1))
    println(fromDefault())
    println(opt(1))
    println(new Box("a").get)
    println(new Box(a = "a").get)
    println(new Box[String]("a").get)
    println(new Box3("s")())
    println(new Box3(a = "s")())
    println(new Box3(1)(c = 2))

    // the type argument is inferred from the expected type
    val e1: List[Int] = pick(Nil); println(e1)
    val e2: String = pick(null); println(e2)
    val e3: Option[Int] = pick(None); println(e3)
    val e4: List[Int] = pick(a = Nil); println(e4)
    val e5: List[Int] = pickL(Nil)(); println(e5)
    val e6: String = three(null)(); println(e6)
    val e7: AnyVal = fromDefault(); println(e7)
    val e8: Box[List[Int]] = new Box(Nil); println(e8.get)
    val e9: Box[List[Int]] = new Box(a = Nil); println(e9.get)
    val e10: CC[List[Int]] = CC(Nil); println(e10)
    val e11: Option[String] = byName(None); println(e11)

    // the type argument is inferred from the expected type, also where the default is stored in a
    // local (a qualifier evaluated first, named arguments out of order)
    val s1: String = new B().m("a"); println(s1)
    val s2: List[Int] = new B().m(Nil); println(s2)
    val s3: List[Int] = new B().m(a = Nil); println(s3)
    val s4: List[Int] = new Super().m(Nil); println(s4)
    val s5: List[Int] = laterP(1)(Nil); println(s5)
    val s6: List[Int] = Test.pick(Nil); println(s6)
    val s7: List[Int] = pickR(a = Nil); println(s7)
    val s8: BoxR[List[Int]] = new BoxR(a = Nil); println(s8.get)
    val s9: List[Int] = { val o = new Outer; val b: o.Box[List[Int]] = new o.Box(a = Nil); b.get }; println(s9)
    val s10: List[Int] = byName(a = Nil); println(s10)
    val s11: ListBuffer[Any] = add(1); println(s11)

    // by-name parameter
    println(byName("a"))
    println(byName(a = "a"))

    // implicit parameter list
    println(imp(3))
    println(imp(a = "x"))
    val i1: Option[Int] = impD(None); println(i1)

    // constructors, case class apply and copy
    println(CC("a"))
    println(CC(a = "a"))
    println(CC("a").copy(a = 1))

    println(later(a = 1)("x"))
    println(later(1)(c = "x"))

    // overrides with renamed type parameters
    println(new B().m("a"))
    println(new B().m(a = "a"))
    println(new B2().m("a"))
    println(new B2().m(a = "a"))
  }
}
