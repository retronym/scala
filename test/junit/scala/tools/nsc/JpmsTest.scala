package scala.tools.nsc

import java.io.ByteArrayOutputStream
import java.nio.file._
import java.util.jar.Attributes
import java.util.jar.Attributes.Name

import org.junit.Assert.fail
import org.junit.Test

import scala.tools.nsc
import scala.tools.testing.{AssertUtil, BytecodeTesting, ClearAfterClass, CompilerErrors}

class JpmsTest extends ClearAfterClass {

  private val source = cached("sourceFileFactory", () => new SourceFileFactory())

  @Test def modulePath(): Unit = {
    val javaClassPath = sys.props("java.class.path").split(java.io.File.pathSeparator).toList
    val library = javaClassPath.find(element => Paths.get(element).getFileName.toString == "library").get
    val outputDir = Files.createTempDirectory("test-")
    val scalaLibraryJpmsModuleJar = createDummyAutomaticModule(outputDir, "scala.library")
    def createCompiler(extraArgs: String) = {
      val compiler = BytecodeTesting.newCompiler(extraArgs = s"-modulepath $scalaLibraryJpmsModuleJar -patchmodule:scala.library=$library $extraArgs")
      // Use a regular output directory
      compiler.outputFunction = () => nsc.io.AbstractFile.getDirectory(outputDir.toFile)
      compiler
    }
    val code1 =
      """
        |package p1.impl
        |class C {
        |  def foo = javax.xml.bind.DatatypeConverter.printBase64Binary _
        |}
      """.stripMargin

    val compilerDefault = createCompiler("")

    {
      val thrown = AssertUtil.assertThrows[CompilerErrors](compilerDefault.compileSourceFiles(source(code1) :: Nil))
      thrown.messages.toList match {
        case msg :: Nil => assert(msg.msg.contains("class DatatypeConverter in package bind cannot be accessed in package javax.xml.bind"), msg)
        case msgs => fail(msgs.mkString("\n"))
      }
    }

    val compilerAddModules = createCompiler("-addmodules:java.xml.bind")
    compilerAddModules.compileSourceFiles(source(code1) :: Nil)

    val module1 =
      """
        |module p1 {
        |  requires java.xml.bind;
        |  requires scala.library;
        |  exports p1.api
        |}
      """.stripMargin
    compilerDefault.compileSourceFiles(source(code1) :: source("module-info.java", module1) :: Nil)

    val unittestCode = """package other; class Patched { new p1.impl.C}"""

    {
      val thrown = AssertUtil.assertThrows[CompilerErrors](createCompiler(s"-patchmodule:p1=${outputDir}").compileSourceFiles(source(unittestCode) :: Nil))
      thrown.messages.toList match {
        case msg :: Nil => assert(msg.msg.contains("class C in package impl cannot be accessed in package p1.impl"), msg)
        case msgs => fail(msgs.mkString("\n"))
      }
    }

    val unitTestPatchModuleCompiler = createCompiler(s"-patchmodule:java.xml.bind=${source.baseDirectory} -cp ${outputDir}")
    unitTestPatchModuleCompiler.compileSourceFiles(source("unittest.scala", unittestCode) :: source("module-info.java", module1) :: Nil)

  }

  private def createDummyAutomaticModule(tempDir: Path, value: String) = {
    val zip = tempDir.resolve(value.replaceAllLiterally(".", "-") + ".jar")
    createZip(zip, List("/META-INF/MANIFEST.MF" -> createManifest(value)))
    zip
  }
  private def createManifest(automaticModuleName: String): Array[Byte] = {
    val manifest = new java.util.jar.Manifest()
    manifest.getMainAttributes.put(Name.MANIFEST_VERSION, "1.0")
    manifest.getMainAttributes.put(new Attributes.Name("Automatic-Module-Name"), automaticModuleName)
    val os = new ByteArrayOutputStream()
    manifest.write(os)
    val manifestBytes = os.toByteArray
    manifestBytes
  }
  private def createZip(zipLocation: Path, content: List[(String, Array[Byte])]): Unit = {
    val env = new java.util.HashMap[String, String]()
    Files.deleteIfExists(zipLocation)
    env.put("create", String.valueOf(true))
    val fileUri = zipLocation.toUri
    val zipUri = new java.net.URI("jar:" + fileUri.getScheme, fileUri.getPath, null)
    val zipfs = FileSystems.newFileSystem(zipUri, env)
    try {
      try {
        for ((internalPath, contentBytes) <- content) {
          val internalTargetPath = zipfs.getPath(internalPath)
          Files.createDirectories(internalTargetPath.getParent)
          Files.write(internalTargetPath, contentBytes)
        }
      } finally {
        if (zipfs != null) zipfs.close()
      }
    } finally {
      zipfs.close()
    }
  }
}
