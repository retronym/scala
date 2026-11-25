import scala.tools.nsc.Settings
import scala.tools.nsc.reporters.{Reporter, StoreReporter}
import scala.tools.partest.DirectTest

// How many candidates an implicit search typechecks, and how many specificity tests it makes, when one
// candidate is better than all the others. TC is contravariant and has an instance for each class of the
// chain C0 >: C1 >: ... >: C20, so all 21 instances are eligible for TC[C20] and the one for C0 wins.
object Test extends DirectTest {
  override def extraSettings = "-usejavacp -Ystop-after:typer -Ystatistics-enabled"
  override def reporter(settings: Settings): Reporter = new StoreReporter(settings)

  def code = ""

  val classes = 20
  val searches = 10

  def program(instanceOrder: Seq[Int]) = s"""
    trait TC[-A] { def id: Int }
    class C0
    ${(1 to classes).map(k => s"class C$k extends C${k - 1}").mkString("; ")}
    object Inst {
      ${instanceOrder.map(k => s"implicit val tc$k: TC[C$k] = new TC[C$k] { def id = $k }").mkString("\n      ")}
    }
    object Use {
      import Inst._
      def use = {
        ${List.fill(searches)(s"implicitly[TC[C$classes]]").mkString("\n        ")}
      }
    }
  """

  def count(title: String, instanceOrder: Seq[Int]): Unit = {
    val global = newCompiler()
    val ok = compileString(global)(program(instanceOrder))
    import global.statistics._
    assert(implicitSearchCount.value == searches, implicitSearchCount.value)
    println(s"$title: compiles = $ok, candidates typed per search = ${typedImplicits.value / searches}, " +
      s"at most ${2 * classes} specificity tests per search = ${improvesCount.value / searches <= 2 * classes}")
  }

  def show(): Unit = {
    count("instance for the subclass declared first", classes to 0 by -1)
    count("instance for the base class declared first", 0 to classes)
  }
}
