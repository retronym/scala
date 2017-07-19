package scala.tools.nsc.scaladoc

import org.junit.{Assert, Test}

import scala.tools.nsc.doc
import scala.tools.nsc.doc.html.page.IndexScript

class IndexScriptTest {

  val docFactory = {
    val settings = new doc.Settings({Console.err.println(_)})
    settings.scaladocQuietRun = true
    settings.nowarn.value = true
    SettingsUtil.configureClassAndSourcePath(settings)

    val reporter = new scala.tools.nsc.reporters.ConsoleReporter(settings)
    new doc.DocFactory(reporter, settings)
  }

  val indexModelFactory = doc.model.IndexModelFactory

  def createIndexScript(path: String) = {
    val absolutePath: String = SettingsUtil.checkoutRoot.resolve(path).toAbsolutePath.toString
    docFactory.makeUniverse(Left(List(absolutePath))) match {
      case Some(universe) =>
        Some(new IndexScript(universe))
      case _ =>
        None
    }
  }

  @Test def allPackages(): Unit = {
    val created = createIndexScript("src/scaladoc/scala/tools/nsc/doc/html/page/IndexScript.scala").get.allPackages.map(_.toString)
    val expected = List(
      "scala",
      "scala.tools",
      "scala.tools.nsc",
      "scala.tools.nsc.doc",
      "scala.tools.nsc.doc.html",
      "scala.tools.nsc.doc.html.page"
    )
    Assert.assertEquals(expected, created)
  }
}
