import scala.tools.partest._
import java.io.File

object Test extends StoreReporterDirectTest {
  def code = ???
  def show(): Unit = {
    val classpath = List(sys.props("partest.lib"), testOutput.path) mkString sys.props("path.separator")
    val compiler = newCompiler("-cp", classpath, "-d", testOutput.path)
    val accessorName = classOf[p1.A].getDeclaredMethods.find(_.isSynthetic).get.getName // access$100
    def code = s"package p1; class Client { def test(a: A) = A.${compiler.TermName(accessorName)}(a)}"
    compileString(compiler)(code)
    assert(storeReporter.errorCount == 1, message = filteredInfos map (_.msg) mkString "; ")
  }
}
