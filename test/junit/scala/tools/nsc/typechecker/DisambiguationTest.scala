package scala.tools.nsc.typechecker

import org.junit.Assert.{assertEquals, assertSame, assertTrue}
import org.junit.Test

import scala.tools.testkit.BytecodeTesting
import scala.tools.testkit.BytecodeTesting.makeSourceFile

class DisambiguationTest {
  private val code =
    """class C {
      |  val c: java.util.function.Consumer[Long] = null
      |  val d: java.util.function.Consumer[java.lang.Long] = c
      |}""".stripMargin

  private def messages(extraArgs: String): List[String] = {
    val compiler = BytecodeTesting.newCompiler(extraArgs = extraArgs)
    import compiler.global
    val run = new global.Run
    run.compileSources(makeSourceFile(code, "C.scala") :: Nil)
    compiler.global.reporter.asInstanceOf[scala.tools.nsc.reporters.StoreReporter].infos.toList.map(_.msg)
  }

  @Test def sameMessageOnAConcurrentCompiler(): Unit = {
    val serial = messages("")
    assertTrue(serial.toString, serial.exists(m => m.contains("scala.Long") && m.contains("java.lang.Long")))
    assertEquals(serial, messages("-Yparallel-typer -Yparallel-typer-threads 2"))
  }

  /** The other threads look `Long` up in the package's scope while this thread renders a message that may be discarded. */
  @Test def noRenameOnAConcurrentCompiler(): Unit = {
    val compiler = BytecodeTesting.newCompiler(extraArgs = "-Yparallel-typer -Yparallel-typer-threads 2")
    import compiler.global._
    new Run
    val consumer = rootMirror.getRequiredClass("java.util.function.Consumer")
    val javaLong = rootMirror.getRequiredClass("java.lang.Long")
    val scalaLong = definitions.LongClass
    val found = appliedType(consumer, scalaLong.tpe)
    val req = appliedType(consumer, javaLong.tpe)
    val rendered = analyzer.withDisambiguation(Nil, found, req) {
      assertSame(tpnme.Long, scalaLong.name)
      assertSame(scalaLong, definitions.ScalaPackageClass.info.decl(tpnme.Long))
      assertSame(javaLong, definitions.JavaLangPackageClass.info.decl(tpnme.Long))
      s"$found $req"
    }
    assertEquals("java.util.function.Consumer[scala.Long] java.util.function.Consumer[java.lang.Long]", rendered)
    assertEquals("java.util.function.Consumer[Long]", found.toString)
  }
}
