package scala.reflect.io

import java.io.IOException
import java.net.{URI, URL}
import java.nio.file.{Files, Path, Paths}
import java.util.jar.{Attributes, Manifest, JarEntry, JarOutputStream}

import org.junit.Assert._
import org.junit.Test

import scala.tools.testkit.AssertUtil._
import scala.tools.testkit.ForDeletion
import scala.tools.testkit.Releasables._
import scala.util.chaining._
import scala.util.Using

class ZipArchiveTest {

  @Test
  def corruptZip(): Unit = {
    val f = Files.createTempFile("test", ".jar")
    val fza = new FileZipArchive(f.toFile)
    try {
      assertThrown[IOException](_.getMessage.contains(f.toString))(fza.iterator)
    } finally {
      Files.delete(f)
    }
  }

  @Test
  def weirdFileAtRoot(): Unit = {
    val f = Files.createTempFile("test", ".jar").tap {f =>
      Using.resource(new JarOutputStream(Files.newOutputStream(f))) { jout =>
        jout.putNextEntry(new JarEntry("/.hey.txt"))
        val bytes = "hello, world".getBytes
        jout.write(bytes, 0, bytes.length)
        ()
      }
    }
    Using.resources(ForDeletion(f), new FileZipArchive(f.toFile)){ (_, fza) =>
      assertEquals(Seq(".hey.txt"), fza.iterator.toSeq.map(_.name))
    }
  }

  @Test
  def missingFile(): Unit = {
    val f = Paths.get("xxx.does.not.exist")
    val fza = new FileZipArchive(f.toFile)
    assertThrown[IOException](_.getMessage.contains(f.toString))(fza.iterator)
  }

  private def manifestAt(location: Path): URL = URI.create(s"jar:file:${location.toUri.getPath}!/META-INF/MANIFEST.MF").toURL

  // ZipArchive.fromManifestURL(URL)
  @Test def `manifest resources just works`(): Unit = {
    val jar = createTestJar()
    Using.resources(ForDeletion(jar), new ManifestResources(manifestAt(jar.toAbsolutePath))) { (_, archive) =>
      val it = archive.iterator
      assertTrue(it.hasNext)
      val f = it.next()
      assertFalse(it.hasNext)
      assertEquals("foo.class", f.name)
    }
  }

  private def createTestJar(): Path = Files.createTempFile("junit", ".jar").tap { f =>
    val man = new Manifest()
    man.getMainAttributes().put(Attributes.Name.MANIFEST_VERSION, "1.0")
    man.getEntries().put("foo.class", new Attributes(0))
    Using.resource(new JarOutputStream(Files.newOutputStream(f), man)) { jout =>
      jout.putNextEntry(new JarEntry("foo.class"))
      val bytes = "hello, world".getBytes
      jout.write(bytes, 0, bytes.length)
      ()
    }
  }

  // ZipArchive.fromURL(URL)
  @Test def `URL archive works`(): Unit = {
    val z = createTestZip()
    Using.resources(ForDeletion(z), new URLZipArchive(z.toUri.toURL)) { (_, zip) =>
      val zit = zip.iterator
      assertTrue(zit.hasNext)
      val f = zit.next()
      assertFalse(zit.hasNext)
      assertEquals("foo.class", f.name)
    }
  }

  private def createTestZip(): Path = Files.createTempFile("junit", ".zip").tap { f =>
    import java.util.zip._
    Using.resource(new ZipOutputStream(Files.newOutputStream(f))) { zout =>
      zout.setLevel(Deflater.NO_COMPRESSION)
      zout.setMethod(ZipOutputStream.STORED)
      val entry = new ZipEntry("foo.class")
      val bytes = "hello, world".getBytes
      entry.setSize(bytes.length)
      entry.setCompressedSize(bytes.length)
      entry.setCrc(new CRC32().tap(_.update(bytes, 0, bytes.length)).getValue)
      zout.putNextEntry(entry)
      zout.write(bytes, 0, bytes.length)
      zout.closeEntry()
      ()
    }
  }
  private val contents = List(
    "foo.class"   -> "hello, world".getBytes,
    "empty.txt"   -> Array.emptyByteArray,
    "big.bin"     -> Array.tabulate[Byte](100000)(i => (i * 31 / 7).toByte),
    "p/q/bar.txt" -> "bar".getBytes,
  )

  private def createZip(stored: Boolean): Path = Files.createTempFile("junit", ".zip").tap { f =>
    import java.util.zip._
    Using.resource(new ZipOutputStream(Files.newOutputStream(f))) { zout =>
      for ((name, bytes) <- contents) {
        val entry = new ZipEntry(name)
        if (stored) {
          entry.setMethod(ZipEntry.STORED)
          entry.setSize(bytes.length)
          entry.setCompressedSize(bytes.length)
          entry.setCrc(new CRC32().tap(_.update(bytes, 0, bytes.length)).getValue)
        }
        // otherwise deflated: the sizes follow the data in a data descriptor, and ZipInputStream reports -1
        zout.putNextEntry(entry)
        zout.write(bytes, 0, bytes.length)
        zout.closeEntry()
      }
    }
  }

  private def checkURLArchive(stored: Boolean): Unit = {
    val z = createZip(stored)
    Using.resources(ForDeletion(z), new URLZipArchive(z.toUri.toURL)) { (_, zip) =>
      def files(dir: AbstractFile): List[AbstractFile] =
        dir.iterator.toList.flatMap(f => if (f.isDirectory) files(f) else List(f))
      val found = zip.iterator.toList.flatMap(f => if (f.isDirectory) files(f) else List(f)).map(f => f.path -> f).toMap
      assertEquals(contents.map(_._1).toSet, found.keySet)
      for ((name, bytes) <- contents) {
        val f = found(name)
        assertEquals(name, Some(bytes.length), f.sizeOption)
        assertArrayEquals(name, bytes, f.toByteArray)
        assertArrayEquals(name, bytes, f.unsafeToByteArray)
        Using.resource(f.input) { in =>
          assertEquals(name, bytes.length, in.available)
          val out = new java.io.ByteArrayOutputStream
          val buf = new Array[Byte](1024)
          var n = in.read(buf)
          while (n >= 0) { out.write(buf, 0, n); n = in.read(buf) }
          assertArrayEquals(name, bytes, out.toByteArray)
        }
      }
    }
  }

  @Test def `URL archive reads stored entries`(): Unit = checkURLArchive(stored = true)

  @Test def `URL archive reads deflated entries`(): Unit = checkURLArchive(stored = false)

  @annotation.nowarn("cat=deprecation")
  @Test def `URL archive closes the stream it opens`(): Unit = {
    val z = createZip(stored = false)
    var opened, closed = 0
    val handler = new java.net.URLStreamHandler {
      override def openConnection(url: URL): java.net.URLConnection = new java.net.URLConnection(url) {
        override def connect() = ()
        override def getInputStream = {
          opened += 1
          new java.io.FilterInputStream(Files.newInputStream(z)) {
            override def close(): Unit = { closed += 1; super.close() }
          }
        }
      }
    }
    Using.resources(ForDeletion(z), new URLZipArchive(new URL(null, "test:archive.zip", handler))) { (_, zip) =>
      assertEquals(4, zip.iterator.size)
      assertEquals(1, opened)
      assertEquals(1, closed)
    }
  }

  /* zipfs doesn't write size field in file header as required by URLZipArchive
  private def createTestZip2(): Path = {
    import java.nio.file.FileSystems
    import java.net.URI
    import scala.util.chaining._
    import scala.jdk.CollectionConverters._
    val f = Files.createTempFile("junit", ".zip")
    Files.delete(f)
    val uri = URI.create(s"jar:${f.toUri}")
    val env = Map("create" -> "true").asJava
    Using.resource(FileSystems.newFileSystem(uri, env)) { fs =>
      val path = fs.getPath("foo.class")
      val bytes = "hello, world".getBytes
      Files.write(path, bytes)
    }
    f.tap(println(_))
  }
   */
}
