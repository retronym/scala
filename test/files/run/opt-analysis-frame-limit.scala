import scala.tools.partest.DirectTest

// A method with a few hundred exception handlers, after inlining, makes ASM's Analyzer allocate
// more than Int.MaxValue bytes of frames in total, though little of it is live at once. ASM counts
// those bytes and throws LimitExceededException; the optimizer must not crash on it.
object Test extends DirectTest {
  override def extraSettings = "-usejavacp -opt:inline:** -opt:local"

  def line(i: Int) =
    s"try { val r: Int = k.f($i).map(x => x + $i).filter(_ > 0).map(_ * 2).getOrElse(0); sb.append(r) } catch { case e: Throwable => sb.append(e) }"

  override def code =
    s"""class K { def f(i: Int): Option[Int] = Some(i) }
       |object C {
       |  def run(k: K): String = {
       |    val sb = new StringBuilder
       |    ${(1 to 200).map(line).mkString("\n    ")}
       |    sb.toString
       |  }
       |}""".stripMargin

  override def show(): Unit = println(compile())
}
