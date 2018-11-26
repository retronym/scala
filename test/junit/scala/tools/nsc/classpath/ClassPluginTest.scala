/*
 * Copyright (c) 2018 Lightbend. All rights reserved.
 */
package scala.tools.nsc.classpath

import java.nio.ByteBuffer

import org.junit.Assert.assertEquals
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.reflect.io.VirtualDirectory
import scala.tools.nsc.backend.{ClassfileInfo, ScalaClass}
import scala.tools.nsc.io.AbstractFile
import scala.tools.nsc.symtab.SymbolTableForUnitTesting
import scala.tools.nsc.util.ClassPath
import scala.tools.testing.BytecodeTesting
import scala.tools.testing.BytecodeTesting.makeSourceFile

@RunWith(classOf[JUnit4])
class ClassPluginTest extends BytecodeTesting {
  // We use this.compiler to generate Scala pickles...
  override def compilerArgs = "-Ystop-after:pickler"

  // ... and this one to read them with a ClassPathPlugin
  object symbolTable extends SymbolTableForUnitTesting {
    val fakeClasses = Map(
      "fake.C" -> ScalaClass("fake.C", () => pickleOf("package fake; class C { def foo = 42 }"))
    )
    private val fakes = new VirtualDirectory("fakes", None)
    fakes.subdirectoryNamed("fake").fileNamed("C.class")

    lazy val classpathPlugin = new platform.ClassPathPlugin {
      override def modifyClassPath(classPath: Seq[ClassPath]): Seq[ClassPath] = {
        // Add a classpath entry with the fake/C.class
        VirtualDirectoryClassPath(fakes) +: classPath
      }

      override def info(file: AbstractFile, clazz: ClassSymbol): Option[ClassfileInfo] =
        fakeClasses.get(clazz.fullNameString)
    }
    this.platform.addClassPathPlugin(classpathPlugin)
  }

  @Test def classPathPluginTest(): Unit = {
    import symbolTable._
    val CClass = rootMirror.getRequiredClass("fake.C")
    val C_tpe = CClass.info
    assertEquals("def foo: Int", definitions.fullyInitializeSymbol(C_tpe.decl(TermName("foo"))).defString)
  }

  private def pickleOf(code: String): ByteBuffer = {
    import compiler._
    val run = newRun
    run.compileSources(makeSourceFile(code, "unitTestSource.scala") :: Nil)
    val pickle = run.symData.toList.head._2
    ByteBuffer.wrap(pickle.bytes, 0, pickle.writeIndex)
  }
}
