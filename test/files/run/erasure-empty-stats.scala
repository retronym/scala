import scala.tools.partest.DirectTest

// Erasure pre-erases a type definition to `EmptyTree`, and drops it from the enclosing template or block.
object Test extends DirectTest {
  override def extraSettings: String = "-usejavacp -Vprint:erasure"

  override def code = "trait T { type A; def f = { type B = Int; 1 } }"

  override def show(): Unit = compile()
}
