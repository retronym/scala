import java.lang.ref.WeakReference

// Each method returns an instance of a local class that does not refer to the Host.
class Host {
  val bytes = new Array[Byte](1 << 16)

  def plain: AnyRef              = { class L(val x: Int) { def f = x }; new L(1) }
  def caseClass: AnyRef          = { case class L(x: Int); L(1) }
  def caseClassCompanion: AnyRef = { case class L(x: Int); L }
  def withLocalSubclass: AnyRef  = { class L { def f = 1 }; class M extends L; new M }
  def localTrait: AnyRef         = { trait T { def f = 1 }; new T {} }
  def localTraitImpl: AnyRef     = { trait T { def f = 1 }; class L extends T; new L }
}

object Test {
  // in a method of its own so that no local variable of `check` refers to the host
  def construct(make: Host => AnyRef): (AnyRef, WeakReference[Host]) = {
    val host = new Host
    (make(host), new WeakReference(host))
  }

  def check(name: String)(make: Host => AnyRef): Unit = {
    val (instance, ref) = construct(make)
    var i = 0
    while (i < 50 && ref.get != null) {
      System.gc()
      Thread.sleep(20)
      i += 1
    }
    println(s"$name: ${if (ref.get == null) "released" else "retained"}")
    assert(instance != null)
  }

  def main(args: Array[String]): Unit = {
    check("local class")(_.plain)
    check("local case class")(_.caseClass)
    check("companion of a local case class")(_.caseClassCompanion)
    check("local class with a local subclass")(_.withLocalSubclass)
    check("anonymous class of a local trait")(_.localTrait)
    check("local class extending a local trait")(_.localTraitImpl)
  }
}
