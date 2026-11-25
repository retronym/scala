import scala.tools.nsc.Settings
import scala.tools.nsc.reporters.{Reporter, StoreReporter}
import scala.tools.partest.DirectTest

// The number of implicit searches needed to find out that a derivation fails, when each level of the
// type can be derived by two routes. `opt` derives Enc[Option[A]] from Enc[A]; `viaSome` and `some`, in a
// parent of lower priority, do so through Enc[Some[A]]. There is no Enc[String].
object Test extends DirectTest {
  override def extraSettings = "-usejavacp -Ystop-after:typer -Ystatistics-enabled"
  override def reporter(settings: Settings): Reporter = new StoreReporter(settings)

  def code = ""

  def program(depth: Int, bottom: String) = s"""
    trait Enc[A]
    trait LowEnc {
      implicit def viaSome[A](implicit e: Enc[Some[A]]): Enc[Option[A]] = new Enc[Option[A]] {}
      implicit def some[A](implicit e: Enc[A]): Enc[Some[A]] = new Enc[Some[A]] {}
    }
    object Enc extends LowEnc {
      implicit val int: Enc[Int] = new Enc[Int] {}
      implicit def opt[A](implicit e: Enc[A]): Enc[Option[A]] = new Enc[Option[A]] {}
    }
    object Use { def x = implicitly[Enc[${"Option[" * depth}$bottom${"]" * depth}]] }
  """

  // (compiles, number of implicit searches)
  def searches(depth: Int, bottom: String): (Boolean, Int) = {
    val global = newCompiler()
    val ok = compileString(global)(program(depth, bottom))
    (ok, global.statistics.implicitSearchCount.value)
  }

  def show(): Unit = {
    for (depth <- List(2, 4, 8)) {
      val (ok, n) = searches(depth, "Int")
      println(s"depth $depth, Enc[Int] at the bottom: compiles = $ok, $n searches")
    }
    // Each level has the three types Enc[Option[T]], Enc[Some[T]] and Enc[T] to look for, so a search that
    // looked for each type once would make about 3 * depth searches.
    for (depth <- List(2, 4, 8, 12)) {
      val (ok, n) = searches(depth, "String")
      println(s"depth $depth, no Enc[String]: compiles = $ok, at most ${4 * depth} searches = ${n <= 4 * depth}")
    }
  }
}
