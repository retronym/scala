package scala.tools.nsc
package classpath

import org.junit.{Assert, Test}
import java.nio.file._
import java.nio.file.attribute.FileTime
import java.util.concurrent.{CountDownLatch, TimeUnit}

import scala.reflect.io.AbstractFile
import scala.tools.testing.BytecodeTesting

class ZipAndJarFileLookupFactoryTest {
  @Test def cacheInvalidation(): Unit = {
    if (scala.util.Properties.isWin) return // can't overwrite an open file on windows.

    val f = Files.createTempFile("test-", ".jar")
    Files.delete(f)
    val g = new scala.tools.nsc.Global(new scala.tools.nsc.Settings())
    assert(!g.settings.YdisableFlatCpCaching.value) // we're testing with our JAR metadata caching enabled.
    def createCp = ZipAndJarClassPathFactory.create(AbstractFile.getFile(f.toFile), g.settings)
    try {
      createZip(f, Array(), "p1/C.class")
      createZip(f, Array(), "p2/X.class")
      createZip(f, Array(), "p3/Y.class")
      val cp1 = createCp
      assert(cp1.findClass("p1.C").isDefined)

      // We expect get a cache hit as the underlying zip hasn't changed
      val cp2 = createCp
      assert(cp2 eq cp1)

      // check things work after the cache hit
      cp1.findClassFile("p2.X").get.toByteArray

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

  @Test def referenceCounting(): Unit = {
    val f = Files.createTempFile("test-", ".jar")
    Files.delete(f)
    val pool = java.util.concurrent.Executors.newFixedThreadPool(8)
    case class TestCompiler() {
      val compiler = BytecodeTesting.newCompiler(extraArgs = s"-classpath $f")
      val g = compiler.global
      g.RunListener.register(new g.RunListener {
        override def compileStarted(): Unit = runStartLatch.await(10, TimeUnit.SECONDS)
        override def compileFinished(): Unit = runEndLatch.await(10, TimeUnit.SECONDS)
      })
      assert(!g.settings.YdisableFlatCpCaching.value) // we're testing with our JAR metadata caching enabled.

      val runStartLatch = new CountDownLatch(1)
      val runStartedLatch = new CountDownLatch(1)
      val runEndLatch = new CountDownLatch(1)
      def releaseRunStart() {runStartLatch.countDown()}
      def releaseRunEndLatch() {runEndLatch.countDown()}
      def compileOnThread(): Unit = {
        val r: Runnable = () => {
          compiler.compileClass("package p1; class Foo")
        }
        pool.submit(r)
      }
    }
    try {
      createZip(f, Array(), "p1/C.class")
      createZip(f, Array(), "p2/X.class")
      createZip(f, Array(), "p3/Y.class")
      val g1 = new TestCompiler
      val g2 = new TestCompiler
      g1.compileOnThread()
      g2.compileOnThread()
      g1.releaseRunStart()
      g2.releaseRunStart()
      Thread.sleep(1000)
      println(ZipAndJarClassPathFactory.referenceCounts)
      Assert.assertEquals(2, ZipAndJarClassPathFactory.referenceCounts(f))
      g2.releaseRunEndLatch()
      Thread.sleep(1000)
      Assert.assertEquals(1, ZipAndJarClassPathFactory.referenceCounts(f))
      g1.releaseRunEndLatch()
      Thread.sleep(1000)
      Assert.assertEquals(0, ZipAndJarClassPathFactory.referenceCounts(f))
    } finally {
      pool.shutdown()
      pool.awaitTermination(1, TimeUnit.MINUTES)
      Files.delete(f)
    }
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

