import java.io.ByteArrayOutputStream
import scala.tools.partest._

// a cold run of partest takes about 15s for this test on my laptop
object Test extends DirectTest {
  override def extraSettings: String = "-usejavacp"

  // test that we hit the code size limit and error out gracefully
  // 5958 is the magic number (2^16/11 -- each `a(1,2,3,4,5,6)` is 11 bytes of bytecode)
  override def code
    = s"""
      |class BigEnoughToFail {
      |  def a(a: Int, b: Int, c: Int, d: Int, e: Int, f: Int): Unit = {}
      |  def tooLong: Unit = {
      |    ${(1 to 5958) map (_ => "a(1,2,3,4,5,6)") mkString(";")}
      |  }
      |}""".stripMargin.trim

  override def show(): Unit = {
    val baos = new ByteArrayOutputStream()
    Console.withErr(baos) {
      compile()
    }
    val out = baos.toString()
    println(out.linesIterator.take(2).mkString("\n"))
  }
}
