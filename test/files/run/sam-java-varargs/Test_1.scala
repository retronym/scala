// A lambda for a Java varargs SAM receives the array: its parameter is an `Array[T]`, not `T*`.
// Compiled against the class files of the Java interfaces.
object Test {
  def arr(xs: Array[_]) = xs.getClass.getSimpleName + xs.mkString("(", ",", ")")

  class Gen[A] { val v: VS[A] = ts => arr(ts) }

  def main(args: Array[String]): Unit = {
    val f3: F3 = (sep, xs) => xs.mkString(sep)
    println(f3.join("-", "1", "2"))
    println(F3.call(f3))
    println(F3.call(Mixed.f3))
    val f3a: F3 = (sep, xs) => arr(xs)
    println(F3.call(f3a))
    val f3t: F3 = (sep: String, xs: Array[AnyRef]) => xs.length.toString
    println(F3.call(f3t))

    val vs: VS[String] = ts => arr(ts)
    println(vs.m("a", "b"))
    println(VS.callStr(vs))
    println(VS.callArr(vs))

    // Java arrays are reference arrays: `Int` elements arrive boxed.
    val vi: VS[Int] = ts => arr(ts)
    println(vi.m(1, 2))
    val vj: VS[Integer] = ts => arr(ts)
    println(VS.callInt(vj))

    println(VS.callStr(new Gen[String].v))
  }
}
