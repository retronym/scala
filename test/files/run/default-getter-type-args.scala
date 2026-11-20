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

object Test {
  def pick[T](a: T, b: T = null.asInstanceOf[T]): T = if (b == null) a else b
  def pickL[T](a: T)(b: T = null.asInstanceOf[T]): T = if (b == null) a else b
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
