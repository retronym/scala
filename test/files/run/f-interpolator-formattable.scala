// java.util.Formatter calls formatTo for %s on a java.util.Formattable, so f"$x" and f"$x%s" must too

import java.util.{Formattable, Formatter}

object Test extends App {
  object Fm extends Formattable {
    def formatTo(f: Formatter, flags: Int, width: Int, precision: Int): Unit = f.format("formatTo")
    override def toString = "toString"
  }
  val any: Any = Fm

  def check(expected: String, actual: String): Unit = assert(expected == actual, s"expected $expected, got $actual")

  check("formatTo", "%s".format(Fm))
  check("formatTo", f"$Fm")
  check("formatTo", f"$Fm%s")
  check("formatTo 1", f"$Fm%s ${1}%d")
  check("formatTo", f"$any")
  check("formatTo", f"$any%s")
}
