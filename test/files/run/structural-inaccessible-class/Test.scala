import scala.language.reflectiveCalls
import java.nio.file.Files

// Structural calls on instances of classes that are not public, or that are in a JDK package
// that is not open (an InaccessibleObjectException on JDK 16+), go through a public declaration
// of the method in a supertype.

class User { def close(): Unit = println("User.close") }

object Test {
  def loan[T <: { def close(): Unit }, R](t: T)(f: T => R): R = try f(t) finally t.close()

  def t(label: String)(f: => Any): Unit =
    try println(s"$label: $f")
    catch { case e: Exception => println(s"$label: ${e.getClass.getName}") }

  def main(args: Array[String]): Unit = {
    val p = Files.createTempFile("structural", ".txt")
    try {
      Files.write(p, "a\nb\n".getBytes)
      t("Files.newInputStream")(loan(Files.newInputStream(p))(_.read()))
      t("Files.lines")(loan(Files.lines(p))(_.count()))
      // twice, the second call uses the cached method
      for (_ <- 1 to 2)
        t("unmodifiableList.size")((java.util.Collections.unmodifiableList(java.util.Arrays.asList(1, 2)): { def size(): Int }).size())
      t("User")(loan(new User)(_ => "done"))
      t("private class")((J.impl(): { def run(): Unit }).run())
      t("package-private class")(J.hidden().asInstanceOf[{ def answer(): Int }].answer())
    } finally Files.delete(p)
  }
}
