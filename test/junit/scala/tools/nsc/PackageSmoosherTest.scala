package scala.tools.nsc

import org.junit.Test

import java.nio.file.Files

class PackageSmoosherTest {
  @Test
  def test(): Unit = {
    val g = new Global(new Settings)
    g.settings.usejavacp.value = true
    import g._
    val tmpDir = Files.createTempDirectory("test-classes-")

    val code =
      s"""
     package o {
       package platform {
         package a {
           class A
           object `package` {}
           object SomeObject
         }
         object `package` {
           implicit def foo: a.A = null
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

    def compile(enabled: Boolean)(code: String) = {
      settings.Xprint.value = List("all")
      settings.outdir.value = tmpDir.toAbsolutePath.toString
      if (enabled)
        settings.YaliasPackage.value = "o.prime=o.platform" :: Nil
      reporter.reset()
      val r = new Run
      r.compileSources(newSourceFile(code) :: Nil)
      assert(!reporter.hasErrors)
    }

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
  }
}
