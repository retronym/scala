import scala.tools.partest.CompilerTest
import scala.tools.partest.instrumented.Instrumentation._

/** We check if classes put in empty package are properly instrumented */
class Foo1 {
  def someMethod = 0
}

/** We check if classes put in `instrumented` package are properly instrumented */
package instrumented {
  class Foo2 {
    def someMethod = 0
  }
}

/** Tests if instrumentation itself works correctly */
object Test {
  def main(args: Array[String]) {
    startProfiling()
    println(f"${"size"}%10s ${"class"}%20s instance")
    println("=" * 72)

    def logObjectSize(o: AnyRef) =
      println(f"${objectSize(o)}%4d bytes  ${o.getClass.toString.takeRight(30)}%30s $o")
    val compilerTest = new CompilerTest {
      import global._
      def check(source: String, unit: CompilationUnit) {
        val m = global.rootMirror
        import m.universe._

        logObjectSize(typeOf[scala.collection.immutable.List[Int]])
        logObjectSize(typeOf[java.lang.String])
      }
    }
    compilerTest.main(Array())
  }
}
