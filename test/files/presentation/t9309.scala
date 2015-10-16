import scala.reflect.internal.util.BatchSourceFile
import scala.reflect.io.{File, AbstractFile, Directory}
import scala.tools.nsc.Settings
import scala.tools.nsc.reporters.StoreReporter

object Test {
  def main(args: Array[String]) {
    val sourceDir = Directory.makeTemp()
    def write(f: File, content: String): Unit = {
      val mod = f.lastModified
      f.writeAll(content)
      f.jfile.setLastModified(mod + 1)
    }

    val A = (sourceDir / "A.scala").createFile()
    write(A, "package a; class A")  // directory does not correspond to package

    val B = (sourceDir / "B.scala").createFile()
    write(B, "")

    val settings = new Settings(sys.error(_))

    settings.sourcepath.value = sourceDir.jfile.getAbsolutePath
    settings.usejavacp.value = true

    def typecheck(source: String): List[StoreReporter#Info] = {
      write(B, source)
      val sourceFile = new BatchSourceFile(AbstractFile.getFile(B))

      val tester = new scala.tools.nsc.interactive.tests.Tester(1, Array(sourceFile), settings)
      import tester.compiler._

      val storeReporter = new StoreReporter
      tester.compiler.reporter = storeReporter
      val response = new Response[Tree]
      tester.compiler.askLoadedTyped(sourceFile, response)
      val t: Tree = response.get.left.get
      storeReporter.infos.toList
    }

    val infos1 = typecheck("object Foo { new a.A() }")
    assert(infos1.isEmpty, infos1)

    val infos2 = typecheck("object Foo { new a.A() }")
    assert(infos2.isEmpty, infos2)

    write(A, "package a; class A { def foo = 0 }")

    val infos3 = typecheck("object Foo { new a.A().foo }")
    assert(infos3.isEmpty, infos3)

    write(A, "package a; class B")

    val infos4 = typecheck("object Foo { new a.A().foo }")
    assert(infos4.exists(_.toString.contains("type A is not a member of package a")), infos4)
  }
}
