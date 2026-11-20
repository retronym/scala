import scala.reflect.runtime.universe._
import scala.reflect.runtime.{currentMirror => cm}

trait T {
  val v: Int = 1
  var w: String = "w"
  lazy val l: Int = 3
  private[this] val pt: Int = 4
  private val p: Int = 5
  private[this] var ptv: Int = 6
  val abs: Int
  def usePrivates = pt + p + ptv
}
trait U extends T {
  val u: Int = 7
}
class C1 extends T { val abs = 8 }
class C2 extends U { val abs = 9 }
class D1 extends C1
class D2 extends C2 { override val v: Int = 10 }
// the class has a field of its own with the name of a private field of the trait
class E1 extends T { val abs = 0; val p: String = "own" }

abstract class A { val x: Int }
class B extends A { val x = 1 }

object Test extends App {
  def test(obj: T, tpe: Type, name: String, newValue: Any)(read: => Any): Unit = {
    val im = cm.reflect(obj)
    val sym = tpe.decl(TermName(name)).asTerm
    print(s"${obj.getClass.getName}: ${tpe.typeSymbol.name}.$name")
    try {
      val fm = im.reflectField(sym)
      print(s" get: ${fm.get}")
      fm.set(newValue)
      println(s", after set: ${fm.get}, read: $read")
    } catch {
      case ex: Throwable => println(s" ${ex.getClass.getName}: ${ex.getMessage}")
    }
  }

  for (mk <- List(() => new C1, () => new C2, () => new D1, () => new D2)) {
    val o = mk()
    test(o, typeOf[T], "v", 11)(o.v)
    test(o, typeOf[T], "w", "ww")(o.w)
    test(o, typeOf[T], "abs", 12)(o.abs)
    test(o, typeOf[T], "pt", 13)(o.usePrivates)
    test(o, typeOf[T], "p", 14)(o.usePrivates)
    test(o, typeOf[T], "ptv", 15)(o.usePrivates)
    o.l
    test(o, typeOf[T], "l", 16)(o.l)
    o match {
      case u: U => test(u, typeOf[U], "u", 17)(u.u)
      case _ =>
    }
  }

  val e1 = new E1
  test(e1, typeOf[T], "p", 14)(s"${e1.usePrivates} ${e1.p}")

  // FieldMirror.get reads the field of the class that mixes in the trait, not the field that overrides it
  println(cm.reflect(new D2).reflectField(typeOf[D2].decl(TermName("v")).asTerm).get)

  // a deferred getter of a class, implemented by a field of a subclass
  val b = new B
  val bm = cm.reflect(b).reflectField(typeOf[A].decl(TermName("x")).asTerm)
  bm.set(2)
  println((bm.get, b.x))

  // the mirror can be bound to an instance of another class that mixes in the trait
  println(cm.reflect(new C1).reflectField(typeOf[T].decl(TermName("abs")).asTerm).bind(new C2).get)

  // the setter symbol of a trait var gives a mirror for the field, like the getter symbol
  test(new C1, typeOf[T], "w_$eq", "")(())

  // a trait member without a field in the receiver
  trait Const { final val k = 1; def m = 1 }
  val c = new Const {}
  val im = cm.reflect(c)
  for (name <- List("k", "m"))
    try println(im.reflectField(typeOf[Const].decl(TermName(name)).asTerm).get)
    catch { case ex: Throwable => println(s"${ex.getClass.getName}: ${ex.getMessage}") }
}
