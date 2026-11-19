//> using options -Wconf:cat=deprecation:s

class B(val x: Int)

// local object in an argument of the super constructor call
class SuperArg extends B({ object O { val q = 2 }; O.q })

// ... along with its companion class
class SuperArgCompanions extends B({ case class C(c: Int); object C { val x = 42 }; C(17).c + C.x })

// in an argument of the self constructor call
class SelfArg(val p: Int) {
  def this() = this({ object O { val q = 3 }; O.q })
}

// in an early definition
trait T
class Early extends { val j = { object O { val q = 4 }; O.q } } with T

// The accessor of a local object that captures the enclosing instance needs that instance.
class Capturing(z: Int) {
  def direct = { object O { val q = z }; O.q }
  def nested = { object O { object P { val q = z } }; O.P.q }
  def viaLocalDef = { def loc = z; object O { val q = loc }; O.q }
  def inClosure = { object O { val q = z }; List(1).map(_ + O.q).head }
}

trait InTrait {
  def z: Int
  def capturing = { object O { val q = z }; O.q }
  def free = { object O { val q = 7 }; O.q }
}

object Test {
  def main(args: Array[String]): Unit = {
    assert(new SuperArg().x == 2)
    assert(new SuperArgCompanions().x == 59)
    assert(new SelfArg().p == 3)
    assert(new Early().j == 4)

    val c = new Capturing(5)
    assert(c.direct == 5)
    assert(c.nested == 5)
    assert(c.viaLocalDef == 5)
    assert(c.inClosure == 6)

    val t = new InTrait { def z = 6 }
    assert(t.capturing == 6)
    assert(t.free == 7)

    // the accessor of a local object is static if and only if it does not need the enclosing instance
    def accessorIsStatic(cls: Class[_]): Boolean = {
      val ms = cls.getDeclaredMethods.filter(m => m.getName.matches("""O\$\d+""") && m.getParameterCount == 1).sortBy(_.getName)
      assert(ms.nonEmpty, cls)
      val statics = ms.map(m => java.lang.reflect.Modifier.isStatic(m.getModifiers)).distinct
      assert(statics.length == 1, ms.mkString(", "))
      statics.head
    }
    assert(accessorIsStatic(classOf[SuperArg]))
    assert(accessorIsStatic(classOf[SelfArg]))
    assert(accessorIsStatic(classOf[Early]))
    assert(!accessorIsStatic(classOf[Capturing]))
  }
}
