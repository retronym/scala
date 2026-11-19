//> using jvm 9+

import scala.tools.partest.DirectTest
import scala.reflect.internal.util.ScalaClassLoader

// The recipe of `makeConcatWithConstants` holds the literal parts of a concatenation. It is a
// constant pool string and limited to 65535 bytes of modified UTF-8.
object Test extends DirectTest {
  def lit(s: String, n: Int) = "\"" + s * n + "\""

  def code = s"""
    object Main {
      def show(s: String): Unit = println("" + s.length + " " + s.hashCode)

      def main(args: Array[String]): Unit = {
        val x = args.length
        // one byte per character, each literal fits in a recipe but not both
        show(${lit("a", 40000)} + x + ${lit("b", 40000)})
        // three bytes per character
        show(${lit("\u20ac", 15000)} + x + ${lit("\u20a9", 15000)})
        // 65535 bytes of literals, which leaves no room for the argument tags
        show(${lit("\u00e9", 32000)} + x + ${lit("\u00fc", 767)} + x + "a")
        // many literals, each small
        show(${List.tabulate(30)(i => lit(('a' + i % 26).toChar.toString, 3000) + " + x").mkString(" + ")})
        // literals that are passed as constants because they contain the tags of a recipe
        show(${lit("a", 40000)} + x + "\\u0001" + x + ${lit("b", 40000)} + x + "\\u0002")
      }
    }
  """

  // a single literal that is too large for the constant pool
  def tooLarge = s"""object TooLarge { def f(x: Int) = ${lit("a", 70000)} + x }"""

  override def show(): Unit =
    for (release <- List("8", "9")) {
      println(s"-release $release")
      val out = (testOutput / s"release-$release").createDirectory()
      assert(compile("-release", release, "-d", out.path))
      ScalaClassLoader.fromURLs(Seq(out.toURL), getClass.getClassLoader).run("Main", Nil)
      assert(!compileString(newCompiler("-release", release, "-d", out.path))(tooLarge))
    }
}
