package scala.tools.nsc.compilerlab

import java.net.URI
import java.nio.file.{FileSystems, Files, Path}
import java.util.zip.ZipFile

import scala.jdk.CollectionConverters._
import scala.reflect.internal.util.AbstractFileClassLoader
import scala.reflect.io.{AbstractFile, VirtualDirectory}
import scala.tools.nsc.classpath.{AggregateClassPath, DirectoryLookup, NoClassPaths, PackageName, SourceFileEntry, SourceFileEntryImpl, VirtualDirectoryClassPath}
import scala.tools.nsc.classpath.FileUtils._
import scala.tools.nsc.util.ClassPath

/** The compile classpath, read into memory once per JVM: the class files of some JDK modules, a class directory (the
 *  quick library, reflect and compiler) and jars, each as a read-only in-memory directory. A compiler then finds every class without
 *  touching the file system or the process-wide caches of jar indexes and the jrt file system.
 */
final class InMemoryClasspath(val roots: List[VirtualDirectory], val files: Int, val bytes: Long) {
  /** A new `ClassPath` over the snapshot, for one `Global`, then the sources in `sourcePath`, if given. */
  def classPath(sourcePath: Option[AbstractFile]): ClassPath =
    AggregateClassPath(roots.map(VirtualDirectoryClassPath(_)) ::: sourcePath.map(InMemoryClasspath.SourcePath(_)).toList)

  override def toString = f"$files%d files, ${bytes / 1e6}%.0f MB"
}

object InMemoryClasspath {
  /** The sources in an in-memory directory, as `-sourcepath` finds them on disk (`DirectorySourcePath`). */
  private final case class SourcePath(dir: AbstractFile) extends ClassPath with DirectoryLookup[SourceFileEntryImpl] with NoClassPaths {
    type F = AbstractFile
    protected def emptyFiles: Array[AbstractFile] = Array.empty
    protected def getSubDir(packageDirName: String): Option[AbstractFile] =
      Option(AbstractFileClassLoader.lookupPath(dir)(packageDirName.split('/').toIndexedSeq, directory = true))
    protected def listChildren(dir: AbstractFile, filter: Option[AbstractFile => Boolean]): Array[AbstractFile] =
      filter.fold(dir.toArray)(dir.iterator.filter(_).toArray)
    protected def hasChild(dir: AbstractFile, name: String): Boolean = dir.lookupName(name, directory = false) != null
    def getName(f: AbstractFile): String = f.name
    def toAbstractFile(f: AbstractFile): AbstractFile = f
    def isPackage(f: AbstractFile): Boolean = f.isPackage
    protected def createFileEntry(file: AbstractFile): SourceFileEntryImpl = SourceFileEntryImpl(file)
    protected def isMatchingFile(f: AbstractFile, siblingExists: String => Boolean): Boolean = f.isScalaOrJavaSource
    def asURLs = Nil
    def asClassPathStrings = Seq(dir.path)
    override def asSourcePathString: String = dir.path
    private[nsc] def sources(inPackage: PackageName): Seq[SourceFileEntry] = files(inPackage)
  }

  private val cache = new java.util.concurrent.ConcurrentHashMap[Corpus.Key, InMemoryClasspath]

  /** The snapshot for a corpus, shared by corpora with the same classpath. */
  def forCorpus(corpus: Corpus): InMemoryClasspath =
    cache.computeIfAbsent(corpus.classpathKey, k => load(k.jdkModules, if (k.withScala) scalaClasspath else Nil, k.deps))

  /** The quick library, reflect and compiler class directories (`compilerlab.scalaClasspath`), as scala/compiler-benchmark
   *  puts the Scala jars on every corpus's classpath.
   */
  private def scalaClasspath: List[Path] =
    sys.props.getOrElse("compilerlab.scalaClasspath", sys.error("-Dcompilerlab.scalaClasspath not set"))
      .split(java.io.File.pathSeparator).toList.map(Path.of(_))

  /** The `/modules` directory of the JDK whose API the corpora compile against: `compilerlab.jdkHome`, or the running
   *  JDK. Another JDK's image is read through its own `jrt-fs.jar`, so a corpus can be compiled against an older API
   *  (as `-release` would) while the compiler runs on a newer JVM.
   */
  private[compilerlab] lazy val jdkImage: Path = sys.props.get("compilerlab.jdkHome") match {
    case Some(home) =>
      FileSystems.newFileSystem(URI.create("jrt:/"), java.util.Map.of("java.home", home)).getPath("/modules")
    case None => FileSystems.getFileSystem(URI.create("jrt:/")).getPath("/modules")
  }

  def load(jdkModules: List[String], dirs: List[Path], jars: List[Path]): InMemoryClasspath = {
    var files = 0
    var bytes = 0L
    def add(root: AbstractFile, rel: String, data: Array[Byte]): Unit = {
      val parts = rel.split('/').toList
      val dir = parts.init.foldLeft(root)(_.subdirectoryNamed(_))
      val out = dir.fileNamed(parts.last).output
      try out.write(data) finally out.close()
      files += 1
      bytes += data.length
    }
    def wanted(rel: String) = rel.endsWith(".class") && !rel.endsWith("module-info.class") && !rel.startsWith("META-INF/")
    def readTree(root: AbstractFile, base: Path): Unit = {
      val walk = Files.walk(base)
      try for (p <- walk.iterator.asScala if Files.isRegularFile(p)) {
        val rel = base.relativize(p).toString
        if (wanted(rel)) add(root, rel, Files.readAllBytes(p))
      } finally walk.close()
    }

    val jdkRoot = new VirtualDirectory(s"memory-jdk", None)
    if (jdkModules.nonEmpty) {
      val modules = jdkImage
      for (m <- jdkModules) readTree(jdkRoot, modules.resolve(m))
    }
    val dirRoots = dirs.map { d =>
      val root = new VirtualDirectory(s"memory-${d.getFileName}", None)
      readTree(root, d)
      root
    }
    val jarRoots = jars.map { jar =>
      val root = new VirtualDirectory(s"memory-${jar.getFileName}", None)
      val zip = new ZipFile(jar.toFile)
      try for (e <- zip.entries.asScala if !e.isDirectory && wanted(e.getName)) {
        val in = zip.getInputStream(e)
        try add(root, e.getName, in.readAllBytes()) finally in.close()
      } finally zip.close()
      root
    }
    new InMemoryClasspath(jdkRoot :: dirRoots ::: jarRoots, files, bytes)
  }
}
