package scala.tools.nsc.util

import java.net.URL
import java.nio.file._
import java.nio.file.attribute.BasicFileAttributes
import java.util

import javax.tools.JavaFileManager.Location
import javax.tools._

import scala.collection.JavaConverters.{iterableAsScalaIterableConverter, _}
import scala.collection.{immutable, mutable}
import scala.reflect.internal.jpms.ClassOutput.{OutputPathClassOutput, SupplierClassOutput}
import scala.reflect.internal.jpms.FileManagerJava9Api._
import scala.reflect.internal.jpms.JpmsClasspathImpl
import scala.reflect.io.PlainNioFile
import scala.tools.nsc.Settings
import scala.tools.nsc.classpath.PackageNameUtils.separatePkgAndClassNames
import scala.tools.nsc.classpath._
import scala.tools.nsc.io.AbstractFile

final class JpmsClassPath(patches: Map[String, List[String]], val impl: JpmsClasspathImpl) extends ClassPath {
  override def asURLs: Seq[URL] = Nil // TODO
  override def asClassPathStrings: Seq[String] = Nil // TODO
  override def asSourcePathString: String = ""

  private val fileManager = impl.getFileManager
  private def locationAsPaths(location: StandardLocation): IndexedSeq[Path] = getLocationAsPaths(fileManager, location).asScala.toVector
  private val packageIndex = mutable.AnyRefMap[String, JpmsPackageEntry]()
  private val moduleLocations = Array(StandardLocation.MODULE_SOURCE_PATH, StandardLocation.UPGRADE_MODULE_PATH, StandardLocation.SYSTEM_MODULES, StandardLocation.MODULE_PATH, StandardLocation.PATCH_MODULE_PATH)
  indexPackages()

  private case class JpmsPackageEntry(name: String) extends PackageEntry {
    val locations = new java.util.LinkedHashMap[Location, StandardLocation]()
    val parentPackage: String = {
      val ix = name.lastIndexOf(".")
      if (ix > 0) name.substring(0, ix) else ""
    }
  }

  private def collectPackages(location: Location, path: Path): Unit = {
    val visitor = new SimpleFileVisitor[Path] {
      override def preVisitDirectory(dir: Path, attrs: BasicFileAttributes): FileVisitResult = {
        val (fullPackageName, className) = if (path.toString == ".") {
          // Workaround an apparent bug in SimpleFileObject.inferBinaryName which doesn't canonicalize the this.path
          val realDir = dir.toRealPath()
          val realPath = path.toRealPath()
          val relativePath = realDir.relativize(realPath)
          separatePkgAndClassNames(relativePath.toString.replaceAllLiterally(relativePath.getFileSystem.getSeparator, "."))
        } else {
          val dummyClassFile = getJavaFileObjects(fileManager, dir.resolve("dummy.class")).iterator().next
          val binaryName = fileManager.inferBinaryName(location, dummyClassFile)
          separatePkgAndClassNames(binaryName)
        }
        if (fullPackageName.contains("-")) FileVisitResult.SKIP_SUBTREE
        else {
          // TODO JPMS optimize this to avoid temp objects.
          for (prefix <- fullPackageName.split('.').inits) {
            val packageName = prefix.mkString(".")
            packageIndex.getOrElseUpdate(packageName, new JpmsPackageEntry(packageName))
          }
          super.preVisitDirectory(dir, attrs)
        }
      }
    }
    Files.walkFileTree(path, visitor)
  }

  // TODO JPMS it would be good if we could limit this until we know the set of modules in the module graph
  //      so that we could avoid listing unreferenced JARs.
  //
  //      But `new Run()` initializes ObjectClass, etc, which needs to lookup scala._ etc. We don't have a way
  //      to just lookup classes by FQN, we have to populate packages in that FQN as we go with package loader.
  //
  //      Maybe we can make package indexing lazier, by just
  private def indexPackages() {
    for {
      moduleLocation <- moduleLocations
      location1 <- listLocationsForModules(fileManager, moduleLocation).asScala
      location <- location1.asScala
      path <- getLocationAsPaths(fileManager, location).asScala
    } {
      collectPackages(location, path)
    }
    val paths = getLocationAsPaths(fileManager, StandardLocation.CLASS_PATH).asScala
    for (path <- paths) {
      collectPackages(StandardLocation.CLASS_PATH, path)
    }
  }

  private[nsc] def hasPackage(pkg: String): Boolean = {
    packageIndex.contains(pkg)
  }
  private[nsc] def packages(inPackage: String): Seq[PackageEntry] = {
    packageIndex.values.iterator.filter(x => x.parentPackage == inPackage).toArray[PackageEntry]
  }

  private[nsc] def classes(inPackage: String): Seq[ClassFileEntry] = {
    val result = immutable.ArraySeq.untagged.newBuilder[ClassFileEntry]
    for {
      moduleLocation <- moduleLocations
      location1 <- {
        assert(isModuleOrientedLocation(moduleLocation), moduleLocation)
        listLocationsForModules(fileManager, moduleLocation).asScala
      }
      location <- location1.asScala
      moduleName = inferModuleName(fileManager, location)
      jfo <- fileManager.list(location, inPackage, util.EnumSet.of(JavaFileObject.Kind.CLASS), false).asScala
    } {
      val binaryName = fileManager.inferBinaryName(location, jfo)
      val (fullPackageName, className) = separatePkgAndClassNames(binaryName)
      val path = asPath(fileManager, jfo)
      result += JpmsClassFileEntryImpl(new PlainNioFile(path), moduleName, moduleLocation)
    }

    for (jfo <- fileManager.list(StandardLocation.CLASS_PATH, inPackage, util.EnumSet.of(JavaFileObject.Kind.CLASS), false).asScala) {
      val binaryName = fileManager.inferBinaryName(StandardLocation.CLASS_PATH, jfo)
      val (fullPackageName, simpleClassName) = PackageNameUtils.separatePkgAndClassNames(binaryName.replaceAll("/", "."))
      val moduleName = ""
      val path = asPath(fileManager, jfo)
      result += JpmsClassFileEntryImpl(new PlainNioFile(path), moduleName, StandardLocation.CLASS_PATH)
      for (prefix <- fullPackageName.split('.').inits) {
        val packageName = prefix.mkString(".")
        val path = asPath(fileManager, jfo)
        packageIndex.getOrElseUpdate(packageName, new JpmsPackageEntry(packageName))
      }
    }
    result.result()
  }

  override private[nsc] def list(inPackage: String) = ClassPathEntries(packages(inPackage), classes(inPackage))

  private[nsc] def sources(inPackage: String): Seq[SourceFileEntry] = Nil

  override def findClassFile(className: String): Option[AbstractFile] = {
    val (inPackage, classSimpleName) = separatePkgAndClassNames(className)
    classes(inPackage).find(_.name == classSimpleName).map(_.file)
  }
}

object JpmsClassPath {
  def apply(s: Settings): ClassPath = {
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

      allPatches = s.patchModule.value.map(parse).groupBy(_._1).mapValues(_.map(_._2)).to(Map)
      for ((module, patches) <- allPatches) {
        add("--patch-module", s"$module=${patches.mkString(",")}")
      }
    }
    add("--class-path", s.classpath.value)
    val releaseOptional = java.util.Optional.ofNullable(s.release.value).filter(!_.isEmpty)
    val singleOutput = s.outputDirs.getSingleOutput.get
    val classOutput = if (singleOutput.file != null)
      new OutputPathClassOutput(singleOutput.file.toPath)
    else {
      val supplier = () => {
        val file = singleOutput.lookupName("module-info.class", false)
        if (file == null) null
        else file.toByteArray
      }
      new SupplierClassOutput(() => supplier())
    }
    val impl = new JpmsClasspathImpl(releaseOptional, classOutput, javaOptions, s.addModules.value.asJava, s.addExports.value.asJava, s.addReads.value.asJava)
    // TODO JPMS refactor this classpath so that settings are re-read on subsequent runs. Maybe currentClasspath should just be recreated on each new Run?
    new JpmsClassPath(allPatches, impl)
  }

  private val EqualsPattern = """(.*)=(.*)""".r
}

private[nsc] case class JpmsClassFileEntryImpl(file: AbstractFile, override val jpmsModuleName: String, location: StandardLocation) extends ClassFileEntry {
  override val name = FileUtils.stripClassExtension(file.name) // class name

  override def binary: Option[AbstractFile] = Some(file)
  override def source: Option[AbstractFile] = None
}
