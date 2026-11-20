//> using jvm 9+

import scala.tools.partest.DirectTest
import scala.reflect.internal.util.ScalaClassLoader

// An operand of a string concatenation is converted to a string before the next operand is
// evaluated, whether the concatenation is compiled to `StringBuilder` calls or to an
// `invokedynamic` of `StringConcatFactory` (`-release 9` and up).
object Test extends DirectTest {
  def code = """
    class C(var name: String) { override def toString = name }
    final class VC(val c: C) extends AnyVal { override def toString = "vc" + c }
    class Thrower { override def toString: String = throw new RuntimeException("toString") }

    object Main {
      def set(sb: java.lang.StringBuilder, s: String): String = { sb.setLength(0); sb.append(s); "!" }
      def rename(c: C, s: String): String = { c.name = s; "!" }

      def main(args: Array[String]): Unit = {
        val sb = new java.lang.StringBuilder("sb")
        println("<" + sb + { sb.append("X"); "y" } + ">")

        val c = new C("a")
        println("" + c + rename(c, "b") + c + rename(c, "c") + c)
        println(s"$c${rename(c, "d")}$c")

        val any: Any = c
        println("" + any + rename(c, "e") + any)

        val vc = new VC(c)
        println("" + vc + rename(c, "f") + vc)

        val cs: CharSequence = sb
        println("" + cs + set(sb, "t") + cs + set(sb, "u"))

        // operands that are stable once evaluated
        val i: Integer = 1
        val n: Null = null
        var s = "s"
        println(s + i + n + 1L + 'c' + { s = "t"; s } + s)

        // more operands than fit in one `makeConcatWithConstants` call
        println("" +
          c + rename(c, "0") + c + rename(c, "1") + c + rename(c, "2") + c + rename(c, "3") + c + rename(c, "4") +
          c + rename(c, "5") + c + rename(c, "6") + c + rename(c, "7") + c + rename(c, "8") + c + rename(c, "9") +
          c + rename(c, "0") + c + rename(c, "1") + c + rename(c, "2") + c + rename(c, "3") + c + rename(c, "4") +
          c + rename(c, "5") + c + rename(c, "6") + c + rename(c, "7") + c + rename(c, "8") + c + rename(c, "9") +
          c + rename(c, "0") + c + rename(c, "1") + c + rename(c, "2") + c + rename(c, "3") + c + rename(c, "4") +
          c + rename(c, "5") + c + rename(c, "6") + c + rename(c, "7") + c + rename(c, "8") + c + rename(c, "9") +
          c + rename(c, "0") + c + rename(c, "1") + c + rename(c, "2") + c + rename(c, "3") + c + rename(c, "4") +
          c + rename(c, "5") + c + rename(c, "6") + c + rename(c, "7") + c + rename(c, "8") + c + rename(c, "9") +
          c + rename(c, "0") + c + rename(c, "1") + c + rename(c, "2") + c + rename(c, "3") + c + rename(c, "4") +
          c + rename(c, "5") + c + rename(c, "6") + c + rename(c, "7") + c + rename(c, "8") + c + rename(c, "9") +
          c + rename(c, "0") + c + rename(c, "1") + c + rename(c, "2") + c + rename(c, "3") + c + rename(c, "4") +
          c)

        // a failing conversion prevents the evaluation of the operands after it
        try println("" + new Thrower + rename(c, "not evaluated"))
        catch { case e: RuntimeException => println(e.getMessage + " " + c) }
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
