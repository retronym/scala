package scala.tools.nsc

import org.junit.Test
import org.junit.rules.TemporaryFolder

import java.nio.file.Files

class PackageSmoosherTest {
  class Fixture(aliases: List[String]) {
    val g = new Global(new Settings)
    g.settings.usejavacp.value = true
    import g._
    val tmpDir = new TemporaryFolder()
    tmpDir.create()
    def compile(enabled: Boolean)(code: String) = {
      // settings.Xprint.value = List("typer")
      settings.outdir.value = tmpDir.getRoot.getAbsolutePath
      if (enabled)
        settings.YaliasPackage.value = aliases
      reporter.reset()
      val r = new Run
      r.compileSources(newSourceFile(code) :: Nil)
      assert(!reporter.hasErrors)
    }
    def close(): Unit = {
      tmpDir.delete()
    }
  }
  def withFixture(aliases: List[String])(f: Fixture => Unit): Unit = {
    val fixture = new Fixture(aliases)
    try f(fixture)
    finally fixture.close()
  }
  @Test
  def test(): Unit = withFixture("o.prime=o.platform" :: Nil) { f =>
    import f._

    val code =
      s"""
     package o {
       package platform {
         package a {
           class A
           object `package` {}
           object SomeObject
           package c {
             class platform_a_c_Class
           }
         }
         object `package` {
           implicit def foo: a.A = null
           val bar = ""
         }
         object SomeObject

       }
       package prime {
         package b {
           class B
         }
         package a {
           class A2
         }
         package q {
            class Query
         }
         class Query
       }
       }
       """

    compile(enabled = false)(code)
    compile(enabled = true)(
      """
        | package client {
        |   object Client {
        |     new o.platform.a.A
        |     new o.prime.a.A
        |     o.prime.SomeObject
        |     o.prime.a.SomeObject
        |   }
        | }
        |""".stripMargin)
    compile(enabled = true)(
      """
        |import o.platform._
        |import o.prime._
        |class Test {
        |  foo
        |  implicitly[o.platform.a.A]
        |}
        |""".stripMargin)
    compile(enabled = true)(
      """
        |package o.platform.bt
        |object `package` {
        |}
        |object O1
        |trait T1
        |""".stripMargin
    )
    compile(enabled = true)(
      """
        |package o.prime.bt
        |
        |class C2
        |""".stripMargin
    )

    compile(enabled = true)(
      """
        |import o.platform._
        |import o.prime.q._
        |
        |class Test extends Query {
        |
        |}
        |""".stripMargin
      )

    compile(enabled = true)(
      """
        |import o.platform._
        |import o.prime.q._
        |
        |class Test extends Query {
        |   new o.prime.a.c.platform_a_c_Class
        |}
        |""".stripMargin
      )
  }

  @Test
  def testPackageObjectImport(): Unit = withFixture( "o.prime.x.y=o.platform.x.y" :: Nil) { f =>
    import f._

    compile(enabled = false)(
      """
        |package o.platform.x
        |
        |package object y {
        |  val someVal = ""
        |}
        |""".stripMargin)
    compile(enabled = false)(
      """
        |package o.prime.x.y
        |
        |private class Placeholder
        |""".stripMargin)

    compile(enabled = true)(
      """
        |package o.prime.x
        |
        |import o.prime.x.y.{someVal => _, _}
        |
        |class Test {
        |}
        |""".stripMargin)
  }
  @Test
  def testNesting(): Unit = withFixture("o.prime=o.platform" :: Nil) { f =>
    import f._

    compile(enabled = false)(
      """
        |package o.platform.x.y
        |
        |class InXY
        |""".stripMargin)
    compile(enabled = false)(
      """
        |package o.prime
        |
        |private class Placeholder
        |""".stripMargin)

    compile(enabled = true)(
      """
        |package other
        |
        |class Test {
        |  new o.prime.x.y.InXY
        |}
        |""".stripMargin)
  }
}
