import java.lang.ref.WeakReference

final class Big { val bytes = new Array[Byte](1 << 16); def use(): Int = bytes.length }

// The constructor parameter is only needed while the constructor runs.
class Direct(b: Big)     { val n = b.use() }
class TryFinally(b: Big) { val n = try b.use() finally Test.sink(1) }
class TryVal(b: Big)     { val n = try b.use() catch { case _: IllegalStateException => 0 } }
class TryVar(b: Big)     { var n = try b.use() catch { case _: IllegalStateException => 0 } }
class TryInIf(b: Big)    { val n = if (b ne null) { try b.use() catch { case _: IllegalStateException => 0 } } else 0 }

object Test {
  def sink(x: Any): Unit = ()

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
    check("val n = b.use()")(new Direct(_))
    check("val n = try b.use() finally ...")(new TryFinally(_))
    check("val n = try b.use() catch ...")(new TryVal(_))
    check("var n = try b.use() catch ...")(new TryVar(_))
    check("val n = if (c) try b.use() catch ... else 0")(new TryInIf(_))
  }
}
