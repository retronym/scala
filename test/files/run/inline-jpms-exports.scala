//> using jvm 9+
//> using options -opt:inline:**

// `Objects.checkIndex` forwards to `jdk.internal.util.Preconditions.checkIndex`, and
// `Double.parseDouble` to `jdk.internal.math.FloatingDecimal.parseDouble`. Both are public
// classes, but their packages are not exported by java.base, so the forwarders cannot be
// inlined into a class in the unnamed module (scala/bug#11812).
object Test {
  def t1(i: Int, n: Int): Int = java.util.Objects.checkIndex(i, n)
  def t2(s: String): Double = java.lang.Double.parseDouble(s)
  def main(args: Array[String]): Unit = {
    println(t1(1, 2))
    println(t2("1.5"))
  }
}
