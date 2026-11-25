import java.lang.ref.WeakReference

final class Big { val bytes = new Array[Byte](1 << 16); def use(): Int = bytes.length }

// Each method returns a closure or an object that reads a value computed from `big`. The value has been computed before
// the method returns, so nothing can read `big` again.
object Test {
  def sink(x: Any): Unit = ()

  def strictVal(big: Big): AnyRef     = { val l = big.use(); () => l }
  def lazyInt(big: Big): AnyRef       = { lazy val l = big.use(); sink(l); () => l }
  def lazyRef(big: Big): AnyRef       = { lazy val l = "n=" + big.use(); sink(l); () => l }
  def localObject(big: Big): AnyRef   = { object O { val n = big.use() }; sink(O.n); () => O.n }
  def viaLocalDef(big: Big): AnyRef   = { lazy val l = big.use(); def get = l; sink(get); () => get }
  def viaLocalClass(big: Big): AnyRef = { lazy val l = big.use(); class C { def get = l }; val c = new C; sink(c.get); c }

  // in a method of its own so that no local variable of `check` refers to the argument
  def construct(make: Big => AnyRef): (AnyRef, WeakReference[Big]) = {
    val big = new Big
    (make(big), new WeakReference(big))
  }

  def check(name: String)(make: Big => AnyRef): Unit = {
    val (reader, ref) = construct(make)
    var i = 0
    while (i < 50 && ref.get != null) {
      System.gc()
      Thread.sleep(20)
      i += 1
    }
    println(s"$name: ${if (ref.get == null) "released" else "retained"}")
    assert(reader != null)
  }

  def main(args: Array[String]): Unit = {
    check("strict val read by a closure")(strictVal)
    check("forced lazy val (LazyInt) read by a closure")(lazyInt)
    check("forced lazy val (LazyRef) read by a closure")(lazyRef)
    check("initialised local object read by a closure")(localObject)
    check("forced lazy val read through a local def")(viaLocalDef)
    check("forced lazy val read by a local class")(viaLocalClass)
  }
}
