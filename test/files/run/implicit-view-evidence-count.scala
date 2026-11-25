import scala.tools.nsc.Settings
import scala.tools.nsc.reporters.{Reporter, StoreReporter}
import scala.tools.partest.DirectTest

// The number of implicit searches that one call of an extension method costs, when the extension method
// needs evidence that takes six searches to derive (Show[Option[List[(Int, Option[Int])]]]).
object Test extends DirectTest {
  override def extraSettings = "-usejavacp -Ystop-after:typer -Ystatistics-enabled"
  override def reporter(settings: Settings): Reporter = new StoreReporter(settings)

  def code = ""

  def show(calls: Int, imports: String, call: String) = s"""
    trait Show[A] { def show(a: A): String }
    object Show {
      implicit val int: Show[Int] = _.toString
      implicit def opt[A](implicit s: Show[A]): Show[Option[A]] = _.fold("None")(s.show)
      implicit def list[A](implicit s: Show[A]): Show[List[A]] = _.map(s.show).mkString
      implicit def pair[A, B](implicit a: Show[A], b: Show[B]): Show[(A, B)] = p => a.show(p._1) + b.show(p._2)
    }
    object onMethod { implicit class ShowOps[A](private val a: A) extends AnyVal { def show(implicit s: Show[A]): String = s.show(a) } }
    object onView { implicit class ShowOps[A](a: A)(implicit s: Show[A]) { def show: String = s.show(a) } }
    object Use {
      $imports
      type X = Option[List[(Int, Option[Int])]]
      def use(x: X) = {
        ${List.fill(calls)(call).mkString("\n        ")}
        ""
      }
    }
  """

  def numeric(calls: Int, imports: String, call: String) = s"""
    object Use {
      $imports
      def use[T](a: T, b: T)(implicit num: Numeric[T]): T = {
        var acc = a
        ${List.fill(calls)(s"acc = $call").mkString("\n        ")}
        acc
      }
    }
  """

  def searches(program: String): Int = {
    val global = newCompiler()
    val ok = compileString(global)(program)
    assert(ok, global.reporter.asInstanceOf[StoreReporter].infos.mkString("\n"))
    global.statistics.implicitSearchCount.value
  }

  def perCall(title: String, program: (Int, String, String) => String, imports: String, call: String): Unit = {
    val calls = 10
    val n = searches(program(calls, imports, call)) - searches(program(0, imports, call))
    assert(n % calls == 0, n)
    println(s"$title: ${n / calls} searches per call")
  }

  def show(): Unit = {
    perCall("implicitly[Show[X]].show(x)", show, "", "implicitly[Show[X]].show(x)")
    perCall("x.show, evidence is a parameter of the method", show, "import onMethod._", "x.show")
    perCall("x.show, evidence is a parameter of the view", show, "import onView._", "x.show")
    perCall("num.times(a, b)", numeric, "", "num.times(a, b)")
    perCall("a * b through Numeric.Implicits", numeric, "import Numeric.Implicits._", "a * b")
  }
}
