// An object may override a concrete val of a trait. The class that mixes in the trait then
// implements the trait setter, which the trait's initialiser calls, with an empty body.

trait T {
  val f: AnyRef = "T.f"
}
class C extends T {
  override object f { override def toString = "C.f" }
}

// the object is in a subtrait: used to be a ClassFormatError (duplicate method `f`)
trait U extends T {
  override object f { override def toString = "U.f" }
}
class D extends U

// the val is in a class: always worked
class K {
  val f: AnyRef = "K.f"
}
class E extends K {
  override object f { override def toString = "E.f" }
}

// a private object of a trait is not overridden by an object of its name in a subclass
trait P {
  private object f { override def toString = "P.f" }
  def g: String = f.toString
}
class F extends P {
  object f { override def toString = "F.f" }
}

object Test {
  def main(args: Array[String]): Unit = {
    val c = new C
    println(c.f)
    println((c: T).f)

    val anon = new T { override object f { override def toString = "anon.f" } }
    println((anon: T).f)

    val d = new D
    println(d.f)
    println((d: T).f)

    val e = new E
    println(e.f)
    println((e: K).f)

    val p = new F
    println(p.g)
    println(p.f)
  }
}
