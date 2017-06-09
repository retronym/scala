package scala.tools.nsc
package classpath

import org.junit.Test
import java.nio.file._
import java.nio.file.attribute.FileTime

class ZipAndJarFileLookupFactoryTest {
  @Test def cacheInvAalidation(): Unit = {
    val f = Files.createTempFile("test-", ".jar")
    Files.delete(f)
    val g = new scala.tools.nsc.Global(new scala.tools.nsc.Settings())
    def createCp = ZipAndJarClassPathFactory.create(scala.reflect.io.AbstractFile.getFile(f.toFile), g.settings)
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

      // Need to close the `ZipFile` to avoid internal caching it its implementation that exhibits
      // the same problems as we're trying to fix in our layer of caching.
      //
      // Java 9 doesn't have the same problem since http://hg.openjdk.java.net/jdk9/jdk9/jdk/rev/d85c42d008a9
      cp1.close()

      try {
        cp1.findClassFile("p3.Y").get.toByteArray
        assert(false)
      } catch {
        case _: IllegalStateException =>
          // expected: zip file closed, can't open a file
      }

      val lastMod1 = Files.getLastModifiedTime(f)

      // Create a new zip at the same path with different contents and last modified
      Files.delete(f)
      createZip(f, Array(), "p1/D.class")
      Files.setLastModifiedTime(f, FileTime.fromMillis(lastMod1.toMillis + 2000))

      // Our classpath cache should create a new instance
      val cp3 = createCp
      assert(cp1 ne cp3, (System.identityHashCode(cp1), System.identityHashCode(cp3)))
      // And that instance should see D, not C, in package p1.
      assert(cp3.findClass("p1.C").isEmpty)
      assert(cp3.findClass("p1.D").isDefined)
    } finally Files.delete(f)
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

