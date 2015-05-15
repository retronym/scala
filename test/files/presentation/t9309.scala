import java.io.File

import scala.reflect.internal.util.BatchSourceFile
import scala.reflect.io.Directory
import scala.tools.nsc._
import scala.tools.nsc.interactive
import scala.tools.nsc.reporters.StoreReporter

object Test {

  def main(args: Array[String]) {
    val sourceDir = Directory.makeTemp()
    val a = (sourceDir / "a").createDirectory()
    a.jfile.mkdirs()
    val b = (a / "b").createDirectory()
    b.jfile.mkdirs()
    val A = (b / "A.scala").createFile()
    A.createDirectory()
    A.writeAll("package a; class A")

    val settings = new Settings(sys.error(_))

    settings.sourcepath.value = sourceDir.jfile.getAbsolutePath
    settings.uniqid.value = true
    settings.usejavacp.value = true
    settings.stopAfter.value = List("typer")

    val foo = new BatchSourceFile("Foo.scala", "object Foo { new a.A() }")

    val tester = new interactive.tests.Tester(1, Array(foo), settings)

    val storeReporter = new StoreReporter
    tester.compiler.reporter = storeReporter
    tester.compiler.ask { () =>
      val run = new tester.compiler.Run
      run.compileSources(List(foo))
      assert(storeReporter.infos.isEmpty, storeReporter.infos)
      // println(tester.compiler.show(run.units.toList.head.body, printIds = true))
    }
  }
}
