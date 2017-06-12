package scala.tools.nsc
package classpath

import org.junit.{Assert, Test}
import java.nio.file._
import java.nio.file.attribute.FileTime

import org.junit.Assert.{assertNotSame, assertSame}

import scala.tools.nsc.io.AbstractFile
import scala.tools.nsc.util.ClassPath
import scala.tools.testing.AssertUtil

class ZipAndJarFileLookupFactoryTest {
  @Test def cacheInvalidation(): Unit = {
    def test(closeOrSleep: (ClassPath => Unit)): Unit = {
      val f = Files.createTempFile("test-", ".jar")
      Files.delete(f)
      val settings = new scala.tools.nsc.Settings()
      def createCp: ClassPath = ZipAndJarClassPathFactory.create(scala.reflect.io.AbstractFile.getFile(f.toFile), settings)
      try {
        createZip(f, Array(), "p1/C.class")
        createZip(f, Array(), "p2/X.class")
        createZip(f, Array(), "p3/Y.class")
        val cp1 = createCp
        assert(cp1.findClass("p1.C").isDefined)

        // We expect get a cache hit as the underlying zip hasn't changed
        val cp2 = createCp
        assert(cp2 eq cp1)
        cp2.close()

        // Closing the second reference above should not render `cp1` closed.
        cp1.findClassFile("p2.X").get.toByteArray

        closeOrSleep(cp1)

        // Create a new zip at the same path with different contents and last modified
        Files.delete(f)
        createZip(f, Array(), "p1/D.class")

        // Our classpath cache should create a new instance
        val cp3 = createCp
        assert(cp1 ne cp3, (System.identityHashCode(cp1), System.identityHashCode(cp3)))
        // And that instance should see D, not C, in package p1.
        assert(cp3.findClass("p1.C").isEmpty)
        assert(cp3.findClass("p1.D").isDefined)
      } finally Files.deleteIfExists(f)
    }

    // Need to either close the `ZipFile` or wait for the last modified timestamp to tick over
    // to avoid internal caching it its implementation that exhibits
    // the same problems as we're trying to fix in our layer of caching.
    //
    // Testing both variations now. The existing pattern of usage in SBT corresponds ot the `sleep` test,
    // as we're only just adding a `Global.close()` method.
    test(_.close())
    test(_ => Thread.sleep(1500))
  }

  @Test def referenceCountedClosing(): Unit = {
    val f = Files.createTempFile("test-", ".jar")
    Files.delete(f)
    val settings = new scala.tools.nsc.Settings()
    def createCp: ClassPath = ZipAndJarClassPathFactory.create(scala.reflect.io.AbstractFile.getFile(f.toFile), settings)

    createZip(f, Array(), "p1/C.class")
    val cp1 = createCp
    val cp2 = createCp
    assertSame(cp1, cp2)
    cp1.close()
    val cp3 = createCp
    assertSame(cp1, cp3)
    cp2.close()
    cp3.close()
    val cp4 = createCp
    assertNotSame(cp1, cp4)
    AssertUtil.assertThrows[IllegalStateException](cp1.findClassFile("p1.C").get.toByteArray, _.contains("Classpath entry closed"))
    val found: AbstractFile = cp4.findClassFile("p1.C").get
    found.toByteArray
    cp4.close()
    AssertUtil.assertThrows[IllegalStateException](found.toByteArray, _.contains("zip file closed"))
  }

  def createZip(zipLocation: Path, content: Array[Byte], internalPath: String): Unit = {
    val env = new java.util.HashMap[String, String]()
    env.put("create", String.valueOf(Files.notExists(zipLocation)))
    val fileUri = zipLocation.toUri
    val zipUri = new java.net.URI("jar:" + fileUri.getScheme, fileUri.getPath, null)
    val zipfs = FileSystems.newFileSystem(zipUri, env)
    try {
      try {
        val internalTargetPath = zipfs.getPath(internalPath)
        Files.createDirectories(internalTargetPath.getParent)
        Files.write(internalTargetPath, content)
      } finally {
        if (zipfs != null) zipfs.close()
      }
    } finally {
      zipfs.close()
    }
  }
}

