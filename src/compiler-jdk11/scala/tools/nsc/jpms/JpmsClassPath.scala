package scala.tools.nsc.jpms

import java.net.URL
import java.nio.file._
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.spi.FileSystemProvider
import java.util.Collections

import javax.tools.JavaFileManager.Location
import javax.tools.JavaFileObject.Kind
import javax.tools._

import scala.jdk.CollectionConverters._
import scala.collection.{immutable, mutable}
import scala.reflect.internal.jpms.ClassOutput.{OutputPathClassOutput, SupplierClassOutput}
import scala.reflect.internal.jpms.FileManagerJava9Api._
import scala.reflect.internal.jpms.{JpmsClasspathSupport, JpmsModuleDescriptor, ResolvedModuleGraph}
import scala.reflect.io.PlainNioFile
import scala.tools.nsc.Settings
import scala.tools.nsc.classpath.PackageNameUtils.separatePkgAndClassNames
import scala.tools.nsc.classpath._
import scala.tools.nsc.io.AbstractFile
import scala.tools.nsc.util.ClassPath

final class JpmsClassPath(@annotation.unused patches: Map[String, List[String]], private val impl: JpmsClasspathSupport) extends ClassPath {
  private var _sourceModuleDescriptor: JpmsModuleDescriptor = null
  private var _sourceModuleDescriptorSeen = false // for debugging (see NOTE on moduleGraph below)

  // NOTE: do not touch until `jpmsRegisterModuleInfo` has been called from the parser
  private lazy val moduleGraph: ResolvedModuleGraph = {
    _sourceModuleDescriptorSeen = true
    impl.resolveModuleGraph(_sourceModuleDescriptor)
  }

  override private[nsc] def jpmsRegisterModuleInfo(moduleDescriptor: JpmsModuleDescriptor): Unit = {
    assert(!_sourceModuleDescriptorSeen)
    _sourceModuleDescriptor = moduleDescriptor
  }

  override private[nsc] def jpmsModuleNameForSource(srcFile: AbstractFile): String = {
    moduleGraph.moduleForSourceFile(Option(srcFile.file).map(_.toPath).orNull)
  }

  // from a given module's perspective, cache which packages it can see in other modules
  private[this] val accessiblePackages: mutable.Map[String, Map[String, Option[immutable.Set[String]]]] = mutable.Map.empty
  private def computeAccessibility(fromModuleName: String): Map[String, Option[immutable.Set[String]]] = {
    val accessible: java.util.Map[String, java.util.Set[String]] = moduleGraph.accessibleModulePackages(fromModuleName)
    accessible.asScala.map {
      case (targetModule: String, targetPackages: java.util.Set[String]) =>
        val targetPackagesOrWildcard =
          if (targetPackages eq ResolvedModuleGraph.AUTOMATIC_MODULE_EXPORT_ALL) None // this was an automatic module
          else Some(targetPackages.asScala.toSet)
        (targetModule, targetPackagesOrWildcard)
    }.toMap
  }

  override private[nsc] def jpmsIsPackageAccessibleFrom(fromModuleName: String, otherModuleName: String, otherPackageName: String): Boolean = {
    accessiblePackages.getOrElseUpdate(fromModuleName, computeAccessibility(fromModuleName)).get(otherModuleName) match {
      case Some(None)      => true // this was an automatic module
      case Some(Some(set)) => set(otherPackageName)
      case None            => false
    }
  }

  override def asURLs: Seq[URL] = Nil // TODO
  override def asClassPathStrings: Seq[String] = Nil // TODO
  override def asSourcePathString: String = ""

  private val fileManager = impl.getFileManager

  private val moduleLocations = Array(StandardLocation.MODULE_SOURCE_PATH, StandardLocation.UPGRADE_MODULE_PATH, StandardLocation.SYSTEM_MODULES, StandardLocation.MODULE_PATH, StandardLocation.PATCH_MODULE_PATH)
  private val paths = mutable.ArrayBuffer[(Path, StandardLocation, Location)]()
  val jarPackageIndex = mutable.HashMap[String, mutable.ArrayBuffer[Location]]()
  indexLocations()


  lazy val jarfsProvider: Option[FileSystemProvider] = {
    FileSystemProvider.installedProviders.asScala.find(_.getScheme == "jar")
  }

  private def indexLocation(path: Path, standardLocation: StandardLocation, location: Location): Unit = {
    paths.addOne((path, standardLocation, location))
    if (Files.isDirectory(path) || !Files.isRegularFile(path)) {
      () // directories are listed on demand in `packages`; non-existent entries are ignored, as on the class path
    } else {
      val fs = if (impl.getRelease.orElse(null) != null && path.toString.endsWith(".jar")) {
        val env: java.util.Map[String, String] = Collections.singletonMap("multi-release", impl.getRelease.get())
        jarfsProvider.get.newFileSystem(path, env)
      } else FileSystems.newFileSystem(path, null: ClassLoader)
      try {
        for (root <- fs.getRootDirectories.asScala) {
          Files.walkFileTree(root, java.util.EnumSet.noneOf(classOf[FileVisitOption]), Integer.MAX_VALUE, new SimpleFileVisitor[Path]() {
            override def preVisitDirectory(dir: Path, attrs: BasicFileAttributes): FileVisitResult = {
              val dirName = dir.getFileName
              if (dir eq root)
                FileVisitResult.CONTINUE
              else if ((dirName eq null) || dirName.toString.contains("-"))
                FileVisitResult.SKIP_SUBTREE
              else {
                val buffer = jarPackageIndex.getOrElseUpdate(root.relativize(dir).toString.replace('/', '.'), mutable.ArrayBuffer())
                buffer += location
                FileVisitResult.CONTINUE
              }
            }
          })
        }
      } finally {
        fs.close()
      }
    }
  }

  // TODO JPMS It's a bit annoying that we have to start interrogating the classpath before we've resolve the module
  //      graph: we have to assume that all modules are potentially on the module path. It seems pretty hard to
  //      change our initialization order though: `new Run() -> Definitions.init -> PackageLoader.doComplete -> ClassPath.list`
  //      happens before we even have a list of source files, which might include a module-info.java file, which is
  //      relevant to the module graph construction.
  private def indexLocations(): Unit = {
    for {
      moduleLocation <- moduleLocations
      location1 <- listLocationsForModules(fileManager, moduleLocation).asScala
      location <- location1.asScala
      path <- getLocationAsPaths(fileManager, location).asScala
    } {
      indexLocation(path, moduleLocation, location)
    }
    val cpLocation = StandardLocation.CLASS_PATH
    for (path <- getLocationAsPaths(fileManager, cpLocation).asScala)
      indexLocation(path, cpLocation, cpLocation)
  }

  private[nsc] def hasPackage(pkg: PackageName): Boolean =
    pkg.isRoot || jarPackageIndex.contains(pkg.dottedString) || packages(PackageName(PackageNameUtils.separatePkgAndClassNames(pkg.dottedString)._1)).exists(_.name == pkg.dottedString)

  private[nsc] def packages(inPackage0: PackageName): Seq[PackageEntry] = {
    val inPackage = inPackage0.dottedString
    val seen = mutable.Set[String]()
    val result = immutable.ArraySeq.untagged.newBuilder[PackageEntry]
    for {
      (path, standardLocation, location) <- paths
    } {
      // We use NIO path API to list classpath/modulepath elements, rather than FileManager.list which
      // only returns files, not sub-directories. The only way to find sub-directories with FileManager.list
      // is to set `recurse = true`, which needlessly creates JavaFileObject and other metadata for the everything
      // in the classpath.
      val directory = path.resolve(inPackage.replace(".", path.getFileSystem.getSeparator))
      if (Files.isDirectory(directory)) {
        val listing = Files.list(directory).iterator().asScala
        for (file <- listing) {
          if (Files.isDirectory(file)) {
            val fileName = file.getFileName.toString
            if (!seen(fileName) && !fileName.contains("-")) {
              seen += fileName
              val name = if (inPackage == "") fileName else inPackage + "." + fileName
              result += PackageEntryImpl(name)
            }
          }
        }
      }
    }
    for (subJarPackage <- jarPackageIndex.keysIterator.filter(k => PackageNameUtils.packageContains(inPackage, k))) {
      val packageSimpleName = PackageNameUtils.separatePkgAndClassNames(subJarPackage)._2
      if (!seen.contains(packageSimpleName)) {
        seen += packageSimpleName
        result += PackageEntryImpl(subJarPackage)
      }
    }
    result.result()
  }

  private[nsc] def classes(inPackage0: PackageName): Seq[ClassFileEntry] = {
    val inPackage = inPackage0.dottedString
    val result = immutable.ArraySeq.untagged.newBuilder[ClassFileEntry]
    // TODO: simplify indexing logic, but make sure to list each class only once (paths has duplicated entries if you only consider the input to fileManager.list below)
    for ((standardLocation, location) <- paths.map { case (_, sl, l) => (sl, l)}.distinct) {
      val moduleName =
        if (!isModuleOrientedLocation(location)) inferModuleName(fileManager, location)
        else ""
      for (jfo <- fileManager.list(location, inPackage, java.util.EnumSet.of(JavaFileObject.Kind.CLASS), false).asScala) {
        val path = asPath(fileManager, jfo)
        result += JpmsClassFileEntryImpl(new PlainNioFile(path), moduleName, standardLocation)
      }
    }
    result.result()
  }

  override private[nsc] def list(inPackage: PackageName) = ClassPathEntries(packages(inPackage), classes(inPackage))

  private[nsc] def sources(inPackage: PackageName): Seq[SourceFileEntry] = Nil

  override def findClassFile(className: String): Option[AbstractFile] = {
    val (inPackage, classSimpleName) = separatePkgAndClassNames(className)
    val pathsIterator = paths.iterator
    while (pathsIterator.hasNext) {
      val (_, _, location) = pathsIterator.next()
      val it = fileManager.list(location, inPackage, java.util.EnumSet.of(JavaFileObject.Kind.CLASS), false).iterator()
      while (it.hasNext) {
        val jfo = it.next()
        if (jfo.isNameCompatible(classSimpleName, Kind.CLASS))
          return Some(new PlainNioFile(asPath(fileManager, jfo)))
      }
    }
    None
  }

}

object JpmsClassPath {
  def apply(s: Settings, classPath: String): ClassPath = {
    val javaOptions = new java.util.ArrayList[java.util.List[String]]()

    def add(optName: String, args: String*): Unit = {
      javaOptions.add((optName +: args).asJava)
    }

    if (s.modulePath.isSetByUser) {
      add("--module-path", s.modulePath.value)
    }
    var allPatches: Map[String, List[String]] = Map.empty
    if (s.patchModule.isSetByUser) {
      def parse(s: String) = s match {
        case EqualsPattern(module, patch) => (module, patch)
        case _ => throw new IllegalArgumentException(s"Unable to parse argument $s")
      }

      allPatches = s.patchModule.value.map(parse).groupBy(_._1).view.mapValues(_.map(_._2)).toMap
      for ((module, patches) <- allPatches) {
        add("--patch-module", s"$module=${patches.mkString(",")}")
      }
    }
    add("--class-path", classPath)
    val releaseOptional = java.util.Optional.ofNullable(s.releaseValue.orNull)
    val singleOutput = s.outputDirs.getSingleOutput.get
    val classOutput = if (singleOutput.file != null)
      new OutputPathClassOutput(singleOutput.file.toPath)
    else {
      val supplier = () => {
        val file = singleOutput.lookupName("module-info.class", directory = false)
        if (file == null) null
        else file.toByteArray
      }
      new SupplierClassOutput(() => supplier())
    }
    val impl = new JpmsClasspathSupport(releaseOptional, classOutput, javaOptions, s.addModules.value.asJava, s.addExports.value.asJava, s.addReads.value.asJava)
    // TODO JPMS refactor this classpath so that settings are re-read on subsequent runs. Maybe currentClasspath should just be recreated on each new Run?
    if (s.Ylogcp.value) {
      println(s"JPMS classpath ($releaseOptional, $classOutput, $javaOptions, ${s.addModules.value}, ${s.addExports.value}, ${s.addReads.value}) ")
    }
    new JpmsClassPath(allPatches, impl)
  }

  private val EqualsPattern = """(.*)=(.*)""".r
}

private[nsc] case class JpmsClassFileEntryImpl(file: AbstractFile, override val jpmsModuleName: String, location: StandardLocation) extends ClassFileEntry {
  override final def fileName: String = file.name
  override lazy val name = FileUtils.stripClassExtension(file.name) // class name

  override def binary: Option[AbstractFile] = Some(file)
  override def source: Option[AbstractFile] = None
}
