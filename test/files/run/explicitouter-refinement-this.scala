import scala.tools.partest._

// The type argument of the type test refers to the refinement class, which is not an outer class of D:
// only references to enclosing classes are rewritten to outer paths.
object Test extends DirectTest {

  override def extraSettings: String = "-usejavacp -Vprint:explicitouter"

  override def code = """class C {
                        |  class D {
                        |    def f(x: Any) = x.isInstanceOf[AnyRef { type A; def foo: List[A]; def me: this.type }]
                        |    def g(x: Any) = x.isInstanceOf[C.this.type]
                        |  }
                        |}
                        |""".stripMargin.trim

  override def show(): Unit = compile()
}
