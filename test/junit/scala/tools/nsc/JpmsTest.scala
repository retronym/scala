package scala.tools.nsc

import java.io.{ByteArrayOutputStream, File}
import java.nio.file.{FileSystems, Files, Path, Paths}
import java.util.jar.Attributes
import java.util.jar.Attributes.Name

import org.junit.Assert.fail
import org.junit.Test

import scala.tools.testing.{AssertUtil, BytecodeTesting, CompilerErrors}

class JpmsTest {
  @Test def modulePath(): Unit = {
    val javaClassPath = sys.props("java.class.path").split(java.io.File.pathSeparator).toList
    val library = javaClassPath.find(element => Paths.get(element).getFileName.toString == "library").get
    val path = Files.createTempDirectory("test-")
    val scalaLibraryJpmsModuleJar = createDummyAutomaticModule(path, "scala.library")
    val compiler = BytecodeTesting.newCompiler(extraArgs = s"-modulepath $scalaLibraryJpmsModuleJar -patchmodule:scala.library=$library")
    val code1 =
      """
        |class C {
        |  def foo = javax.xml.bind.DatatypeConverter.printBase64Binary _
        |}
      """.stripMargin
    val module1 =
      """
        |module p1 {
        |  requires scala.library
        |}
      """.stripMargin
    val thrown = AssertUtil.assertThrows[CompilerErrors](compiler.compileClass(code1))
    thrown.messages.toList match {
      case msg :: Nil => assert(msg.msg.contains("class DatatypeConverter in package bind cannot be accessed in package javax.xml.bind"), msg)
      case msgs => fail(msgs.mkString("\n"))
    }
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
