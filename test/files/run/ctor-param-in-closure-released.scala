import java.lang.ref.WeakReference

final class Big { val bytes = new Array[Byte](1 << 16); def use(): Int = bytes.length }

// The constructor parameter is only needed while the constructor runs. Nothing that refers to it outlives the constructor.
class Direct(b: Big)       { val n = b.use() }
class InLambda(b: Big)     { val n = List(1).map(_ + b.use()).head }
class InFor(b: Big)        { val n = (for (i <- 1 to 1) yield b.use() + i).head }
class InLocalDef(b: Big)   { val n = { def loc = b.use(); loc } }
class InLocalLazy(b: Big)  { val n = { lazy val l = b.use(); l } }
class InLocalClass(b: Big) { val n = { class L { def f = b.use() }; (new L).f } }

object Test {
  // in a method of its own so that no local variable of `check` refers to the argument
  def construct(make: Big => AnyRef): (AnyRef, WeakReference[Big]) = {
    val big = new Big
    (make(big), new WeakReference(big))
  }

  def check(name: String)(make: Big => AnyRef): Unit = {
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
    check("used directly")(new Direct(_))
    check("used in a lambda")(new InLambda(_))
    check("used in a for comprehension")(new InFor(_))
    check("used in a local def")(new InLocalDef(_))
    check("used in a local lazy val")(new InLocalLazy(_))
    check("used in a local class")(new InLocalClass(_))
  }
}
