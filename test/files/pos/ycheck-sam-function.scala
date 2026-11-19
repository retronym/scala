//> using options -Ycheck:erasure,lambdalift,mixin

// The tree checker types the function again after erasure, when its type is the erased SAM type.
object T {
  def f(x: Int) = { val r: Runnable = () => println(x); r }
  def g(x: Int): java.util.function.Function[String, String] = s => s * x
  def h: java.util.Comparator[String] = (a, b) => a.compareTo(b)
}
