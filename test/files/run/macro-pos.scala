import scala.tools.partest._
import java.io.File
import scala.tools.nsc.util.{ stringFromStream }


object Test extends DirectTest {
  def code = ???

  def compileCode(code: String, opts: Seq[String] = Seq()) = {
    val libReflect = List("lib", "reflect").map(s => sys.props(s"partest.$s"))
    val classpath = (libReflect :+ testOutput.path) mkString sys.props("path.separator")
    compileString(newCompiler(Seq("-cp", classpath, "-d", testOutput.path) ++ opts : _*))(code)
  }

  def macroCode = """
    import scala.reflect.macros.Context
    import language.experimental._
    object M {
      def impl(c: Context)(a: c.Expr[Any]): c.Expr[Any] = c.universe.reify {
        if ("".isEmpty) a.splice else ???
      }
      def apply(a: Any) = macro impl
    }
                  """

  def client = """
     object Test {
       M {
        println("1")
        println("2")
       }
     }
               """

  def slurp(body: => Unit): String = stringFromStream { stream =>
    Console.withOut(stream) {
      Console.withErr(stream) {
        body
      }
    }
  }

  def show(): Unit = {
    assert(compileCode(macroCode))
    assert(compileCode(client, Seq("-Xprint:typer", "-Xprint-pos")))
  }
}
