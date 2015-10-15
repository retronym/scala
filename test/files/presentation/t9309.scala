import java.io.File

import scala.reflect.internal.util.BatchSourceFile
import scala.reflect.io.Directory
import scala.tools.nsc._
import scala.tools.nsc.interactive
import scala.tools.nsc.reporters.StoreReporter

object Test {

  def main(args: Array[String]) {
    val sourceDir = Directory.makeTemp()

    val b = (sourceDir / "b").createDirectory()
    b.jfile.mkdirs()
    val A = (b / "A.scala").createFile() // directory does not correspond to package
    A.createDirectory()
    A.writeAll("package a; class A")

    val settings = new Settings(sys.error(_))

    settings.sourcepath.value = sourceDir.jfile.getAbsolutePath
    settings.usejavacp.value = true
    settings.stopAfter.value = List("typer")
    def typecheck(source: String): List[StoreReporter#Info] = {
      val foo = new BatchSourceFile("Foo.scala", source)

      val tester = new interactive.tests.Tester(1, Array(foo), settings)
      import tester.compiler._

      val storeReporter = new StoreReporter
      tester.compiler.reporter = storeReporter
      val response = new Response[Tree]
      tester.compiler.askLoadedTyped(foo, response)
      storeReporter.infos.toList
    }

    val infos1 = typecheck("object Foo { new a.A() }")
    assert(infos1.isEmpty, infos1)

    val infos2 = typecheck("object Foo { new a.A() }")
    assert(infos2.isEmpty, infos2)

    Thread.sleep(1000)
    A.writeAll("package a; class A { def foo = 0 }")

    val infos3 = typecheck("object Foo { new a.A().foo }")
    assert(infos3.isEmpty, infos3)

    Thread.sleep(1000)
    A.writeAll("package a; class B")

    // TODO
    // val infos4 = typecheck("object Foo { new a.A().foo }")
    // assert(infos4.exists(_.toString.contains("type A is not a member of package a")), infos4)

  }
}
