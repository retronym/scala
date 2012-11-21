import scala.tools.partest._
import java.io.File

object Test extends StoreReporterDirectTest {
  def code = ???

  def compileCode(code: String) = {
    val classpath = List(sys.props("partest.lib"), testOutput.path) mkString sys.props("path.separator")
    compileString(newCompiler("-cp", classpath, "-d", testOutput.path))(code)
  }

  def code1 = """
    package pack1
    trait T {
      xxx
    }
    object `package` extends T {
      def xxx = 0
    }
  """

  def show(): Unit = {
    compileCode(code1)
    val pack1 = new File(testOutput.path, "pack1")
    val tClass = new File(pack1, "T.class")
    assert(tClass.exists)
    assert(tClass.delete())

    // class file package.scala is detected as broken (parent T is missing)
    // so it is discarded to allow it to be rebuild from source.
    compileCode(code1)
    assert(filteredInfos.isEmpty, filteredInfos)
  }
}
