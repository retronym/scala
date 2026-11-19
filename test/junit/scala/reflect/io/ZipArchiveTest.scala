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
  private def createZip(names: String*): Path = Files.createTempFile("junit", ".zip").tap { f =>
    import java.util.zip._
    Using.resource(new ZipOutputStream(Files.newOutputStream(f))) { zout =>
      names.foreach { name =>
        zout.putNextEntry(new ZipEntry(name))
        if (!name.endsWith("/")) zout.write(name.getBytes)
        zout.closeEntry()
      }
    }
  }

  @Test def `DirEntry lookupName honours the directory flag`(): Unit = {
    val z = createZip("top.txt", "p/A.class", "p/q/B.class")
    Using.resources(ForDeletion(z), new FileZipArchive(z.toFile)) { (_, fza) =>
      val root = fza.root
      val p = root.lookupName("p", directory = true)
      assertNotNull("directory p", p)
      assertTrue(p.isDirectory)
      assertSame(fza.allDirs.get("p/"), p)
      assertNull("p is not a file", root.lookupName("p", directory = false))
      assertNull("top.txt is not a directory", root.lookupName("top.txt", directory = true))
      assertEquals("top.txt", root.lookupName("top.txt", directory = false).path)
      assertEquals("p/A.class", p.lookupName("A.class", directory = false).path)
      assertSame(fza.allDirs.get("p/q/"), p.lookupName("q", directory = true))
      assertEquals(Set("top.txt", "p"), root.iterator.map(_.name).toSet)
    }
  }

  @Test def `AbstractFileClassLoader finds a class in a package of a zip`(): Unit = {
    import scala.reflect.internal.util.AbstractFileClassLoader
    val name = "scala.reflect.io.ZipArchiveTest"
    val path = name.replace('.', '/') + ".class"
    val z = Files.createTempFile("junit", ".zip").tap { f =>
      Using.resource(new java.util.zip.ZipOutputStream(Files.newOutputStream(f))) { zout =>
        zout.putNextEntry(new java.util.zip.ZipEntry(path))
        Using.resource(getClass.getClassLoader.getResourceAsStream(path))(_.transferTo(zout))
        zout.closeEntry()
      }
    }
    Using.resources(ForDeletion(z), new FileZipArchive(z.toFile)) { (_, fza) =>
      val loader = new AbstractFileClassLoader(fza.root, null)
      assertEquals(name, loader.loadClass(name).getName)
    }
  }

  // The zip format allows a file `p/q` beside a directory `p/q/`.
  @Test def `file and directory of the same name are both kept`(): Unit = {
    def check(names: String*): Unit = {
      val z = createZip(names: _*)
      Using.resources(ForDeletion(z), new FileZipArchive(z.toFile)) { (_, fza) =>
        val p = fza.allDirs.get("p/")
        val file = p.lookupName("q", directory = false)
        val dir  = p.lookupName("q", directory = true)
        assertNotNull(s"file p/q in $names", file)
        assertFalse(file.isDirectory)
        assertEquals("p/q", new String(file.toByteArray))
        assertSame(fza.allDirs.get("p/q/"), dir)
        assertEquals("p/q/r", dir.lookupName("r", directory = false).path)
        assertEquals(List("q", "q"), p.iterator.map(_.name).toList)
      }
    }
    check("p/q", "p/q/r")
    check("p/q/r", "p/q")
  }

  @Test def `directory name of consecutive entries is shared`(): Unit = {
    val z = createZip("p/A.class")
    Using.resources(ForDeletion(z), new FileZipArchive(z.toFile)) { (_, fza) =>
      val m = classOf[ZipArchive].getDeclaredMethods.find(_.getName.endsWith("dirNameUsingLast")).get
      m.setAccessible(true)
      def dirName(name: String) = m.invoke(fza, name)
      val first = dirName("p/q/A.class")
      assertEquals("p/q/", first)
      assertSame(first, dirName("p/q/B.class"))
      assertSame(first, dirName("p/q/B$.class"))
      assertEquals("p/q/r/", dirName("p/q/r/C.class"))
      assertEquals("p/", dirName("p/q"))
      assertEquals("/", dirName("top.txt"))
      assertEquals("/", dirName("other.txt"))
      assertEquals("p/qq/", dirName("p/qq/D.class"))
      assertEquals("p/q/", dirName("p/q/E.class"))
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
