//> using options -Ycheck:erasure,posterasure
import _root_._

// `_root_.java` is retyped as `java` after erasure, not as an application of the root package.
object Test {
  def f = java.util.Locale.getDefault().toString
}
