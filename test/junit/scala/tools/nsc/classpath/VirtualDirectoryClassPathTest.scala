/*
 * Copyright (c) 2014 Contributor. All rights reserved.
 */
package scala.tools.nsc.classpath

import java.net.URI
import java.nio.file.{FileSystem, FileSystems, Files, Path}

import com.github.marschall.memoryfilesystem.MemoryFileSystemBuilder
import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.collection.JavaConverters.asScalaIteratorConverter
import scala.reflect.io.VirtualDirectory.id
import scala.reflect.io.{AbstractFile, PlainNioFile, VirtualDirectory}
import scala.tools.nsc.util.ClassPath


@RunWith(classOf[JUnit4])
class VirtualDirectoryClassPathTest {

  @Test
  def virtualDirectoryClassPath_findClassFile(): Unit = {
    val base = new VirtualDirectory("base", None)
    test(base, VirtualDirectoryClassPath(base))
  }

  @Test
  def testVirtualNioFilesystem(): Unit = {
    val (name, fileSystem) = VirtualDirectory.newNioVirtualDirectory("base")
    try {
      val root = fileSystem.getPath("/")
      test(new PlainNioFile(root), NioDirectoryClassPath(fileSystem.getPath("/")))
    } finally {
      fileSystem.close()
    }
  }

  @Test
  def testAnoymousVirtualNioFilesystem(): Unit = {
    val fileSystem = VirtualDirectory.newAnonymousNioVirtualDirectory()
    val root = fileSystem.getPath("/")
    test(new PlainNioFile(root), NioDirectoryClassPath(fileSystem.getPath("/")))
  }

  @Test
  def testAnoymousFileSystemDoesNotLeak(): Unit = {
    val content = new Array[Byte](8 * 1024 * 1024)
    (1 to 1024) foreach  { _ =>
      val fileSystem = VirtualDirectory.newAnonymousNioVirtualDirectory()
      val root: Path = fileSystem.getPath("/")
      Files.write(root.resolve("data"), content)
    }
  }

  private def test(base: AbstractFile, classPath: ClassPath) {
    val p1 = base subdirectoryNamed "p1"
    val p1_Test_class = p1.fileNamed("Test.class")
    val p2 = base subdirectoryNamed "p2"
    val p3 = p2 subdirectoryNamed "p3"
    val p4 = p3 subdirectoryNamed "p4"
    val p4_Test1_class = p4.fileNamed("Test.class")

    assertEquals(Some(p1_Test_class), classPath.findClassFile("p1/Test"))

    assertEquals(None, classPath.findClassFile("p1/DoesNotExist"))
    assertEquals(None, classPath.findClassFile("DoesNotExist"))
    assertEquals(None, classPath.findClassFile("p2"))
    assertEquals(None, classPath.findClassFile("p2/DoesNotExist"))
    assertEquals(None, classPath.findClassFile("p4/DoesNotExist"))

    assertEquals(List("p1", "p2"), classPath.packages("").toList.map(_.name).sorted)
    assertEquals(List(), classPath.packages("p1").toList.map(_.name).sorted)
    assertEquals(List("p2.p3"), classPath.packages("p2").toList.map(_.name).sorted)
    assertEquals(List("p2.p3.p4"), classPath.packages("p2.p3").toList.map(_.name).sorted)
  }
}
