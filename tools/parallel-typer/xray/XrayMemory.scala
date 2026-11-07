// The x-ray's compiler with nothing on disk, after jvm-perf-lab's compiler lab (`InMemoryClasspath`, `InMemoryCompiler`):
// the sources, the classpath (class directories, jars and the JDK's modules) and the sourcepath are read into memory
// once per JVM, and the output goes to an in-memory directory. A recording then has no file system calls in it, which
// on a machine with slow file I/O were a large and misleading part of the class headers, the loaders and the back end.
// In package scala.tools.nsc for `JavaPlatform.currentClassPath`.
package scala.tools.nsc.xray

import java.net.URI
import java.nio.file.{FileSystems, Files, Path, Paths}
import java.util.concurrent.ConcurrentHashMap
import java.util.zip.ZipFile

import scala.jdk.CollectionConverters._
import scala.reflect.internal.util.{AbstractFileClassLoader, BatchSourceFile, SourceFile}
import scala.reflect.io.{AbstractFile, VirtualDirectory}
import scala.tools.nsc.{Global, Settings}
import scala.tools.nsc.classpath.{AggregateClassPath, DirectoryLookup, NoClassPaths, PackageName, SourceFileEntry, SourceFileEntryImpl, VirtualDirectoryClassPath}
import scala.tools.nsc.classpath.FileUtils._
import scala.tools.nsc.reporters.StoreReporter
import scala.tools.nsc.util.ClassPath

object InMemory {
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

  private def add(root: AbstractFile, rel: String, data: Array[Byte]): Unit = {
    val parts = rel.split('/').toList
    val dir = parts.init.foldLeft(root)(_.subdirectoryNamed(_))
    val out = dir.fileNamed(parts.last).output
    try out.write(data) finally out.close()
  }
  private def readTree(root: AbstractFile, base: Path, wanted: String => Boolean): Unit = {
    val walk = Files.walk(base)
    try for (p <- walk.iterator.asScala if Files.isRegularFile(p)) {
      val rel = base.relativize(p).toString
      if (wanted(rel)) add(root, rel, Files.readAllBytes(p))
    } finally walk.close()
  }
  private def isClass(rel: String) = rel.endsWith(".class") && !rel.endsWith("module-info.class") && !rel.startsWith("META-INF/")
  private def isSource(rel: String) = rel.endsWith(".scala") || rel.endsWith(".java")

  /** Every module of the running JDK, in one directory. */
  private[xray] lazy val jdk: VirtualDirectory = {
    val root = new VirtualDirectory("memory-jdk", None)
    // -Dxray.jdk.home: compile against another JDK's API than the one running the compiler
    val fs = Option(System.getProperty("xray.jdk.home")) match {
      case Some(home) => FileSystems.newFileSystem(URI.create("jrt:/"), java.util.Collections.singletonMap("java.home", home))
      case None       => FileSystems.getFileSystem(URI.create("jrt:/"))
    }
    val modules = fs.getPath("/modules")
    val ms = Files.list(modules)
    try ms.iterator.asScala.foreach(m => readTree(root, m, isClass)) finally ms.close()
    root
  }
  private val roots = new ConcurrentHashMap[String, VirtualDirectory]
  private[xray] def classRoot(entry: String): VirtualDirectory = roots.computeIfAbsent(entry, _ => {
    val p = Paths.get(entry)
    val root = new VirtualDirectory(s"memory-${p.getFileName}", None)
    if (Files.isDirectory(p)) readTree(root, p, isClass)
    else if (Files.isRegularFile(p)) {
      val zip = new ZipFile(p.toFile)
      try for (e <- zip.entries.asScala if !e.isDirectory && isClass(e.getName)) {
        val in = zip.getInputStream(e)
        try add(root, e.getName, in.readAllBytes()) finally in.close()
      } finally zip.close()
    }
    root
  })
  private val sourceRoots = new ConcurrentHashMap[String, VirtualDirectory]
  private def sourceRoot(dir: String): VirtualDirectory = sourceRoots.computeIfAbsent(dir, _ => {
    val root = new VirtualDirectory(s"memory-sources-${Paths.get(dir).getFileName}", None)
    readTree(root, Paths.get(dir), isSource)
    root
  })
  private val sources = new ConcurrentHashMap[String, SourceFile]
  private[xray] def source(path: String): SourceFile = sources.computeIfAbsent(path, _ =>
    new BatchSourceFile(AbstractFile.getFile(path), new String(Files.readAllBytes(Paths.get(path)), java.nio.charset.StandardCharsets.UTF_8).toCharArray))

  /** A compiler for `args` (options and source files), its reporter and its sources, all in memory. */
  def compiler(args: List[String]): (Global, StoreReporter, List[SourceFile]) = {
    val s = new Settings(msg => sys.error(msg))
    val (ok, rest) = s.processArguments(args, processAll = true)
    assert(ok, rest)
    s.outputDirs.setSingleOutput(new VirtualDirectory("xray-output", None))
    val sep = java.io.File.pathSeparator
    // then the JVM's own classpath, as the recorder on disk has it (`-usejavacp`): the compiler's snapshot and its jars
    val entries = (if (s.classpath.isSetByUser) s.classpath.value.split(sep).toList else Nil) ::: sys.props("java.class.path").split(sep).toList
    val classes = entries.filter(_.nonEmpty).distinct.map(classRoot)
    // macro implementations are loaded by a class loader, from URLs, which an in-memory classpath has none of
    if (!s.YmacroClasspath.isSetByUser) s.YmacroClasspath.value = entries.filter(_.nonEmpty).distinct.mkString(sep)
    val sourcePath = if (s.sourcepath.isSetByUser) s.sourcepath.value.split(sep).toList.filter(_.nonEmpty).map(d => SourcePath(sourceRoot(d))) else Nil
    val cp = AggregateClassPath((jdk :: classes).map(VirtualDirectoryClassPath(_)) ::: sourcePath)
    val reporter = new StoreReporter(s)
    val g = new Global(s, reporter) {
      override lazy val platform: ThisPlatform = new GlobalPlatform {
        currentClassPath = Some(cp)
        // An in-memory file has no time. Where a class is on the classpath and its source on the sourcepath, the
        // class is used, as on disk where the compiler's snapshot is newer than the sources.
        override def needCompile(bin: AbstractFile, src: AbstractFile): Boolean = false
      }
    }
    (g, reporter, rest.map(source))
  }
}
