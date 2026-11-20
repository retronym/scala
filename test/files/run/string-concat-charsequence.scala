//> using jvm 9+

import scala.tools.partest.DirectTest
import scala.reflect.internal.util.ScalaClassLoader

// A `CharSequence` operand of a string concatenation is converted with `toString`, like any other
// reference, whether the concatenation is compiled to `StringBuilder` calls or to an
// `invokedynamic` of `StringConcatFactory` (`-release 9` and up).
object Test extends DirectTest {
  def code = """
    class Chars extends CharSequence {
      def length = 3
      def charAt(i: Int) = "abc".charAt(i)
      def subSequence(start: Int, end: Int) = "abc".subSequence(start, end)
      override def toString = "toString"
    }

    object Main {
      def main(args: Array[String]): Unit = {
        val chars = new Chars
        val cs: CharSequence = chars
        println("x" + chars)
        println("x" + cs + 1)
        println(s"$cs")
        println(s"x$cs")
        println(String.valueOf(cs))
        println("x" + new java.lang.StringBuilder("sb") + new StringBuffer("buf") + java.nio.CharBuffer.wrap("chars"))
        val nullSb: java.lang.StringBuilder = null
        println("x" + nullSb + new StringBuilder("ssb"))
      }
    }
  """

  override def show(): Unit =
    for (release <- List("8", "9")) {
      println(s"-release $release")
      val out = (testOutput / s"release-$release").createDirectory()
      assert(compile("-release", release, "-d", out.path))
      ScalaClassLoader.fromURLs(Seq(out.toURL), getClass.getClassLoader).run("Main", Nil)
    }
}
