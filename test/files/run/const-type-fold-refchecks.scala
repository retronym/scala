import scala.tools.partest._

object Test extends DirectTest {

  override def extraSettings: String = "-usejavacp -Xprint:typer,refchecks -d " + testOutput.path

  override def code = """
                        | object O { final val x = 22; final val y = "" }
                        | class ann(a: Any) extends annotation.StaticAnnotation
                        | @ann(O.x)
                        | class Test {
                        |
                        |   O.x
                        |   println(1 + 1)
                        |   println(1 / 0)
                        |
                        |   @ann(43-1) def bar = 0
                        |
                        |   @ann(1/0)
                        |   def foo = 1/0
                        | }
                        |""".stripMargin.trim

  override def show(): Unit = {
    Console.withErr(System.out) {
      compile()
    }
  }
}
