package scala.tools.nsc.util

import java.net.URL
import java.nio.file._
import java.nio.file.attribute.BasicFileAttributes
import java.util

import javax.tools.JavaFileManager.Location
import javax.tools._

import scala.collection.JavaConverters.{iterableAsScalaIterableConverter, _}
import scala.collection.mutable
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

  private case class JpmsPackageEntry(name: String) extends PackageEntry {
    val locations = new java.util.LinkedHashMap[Location, StandardLocation]()
    val parentPackage: String = {
      val ix = name.lastIndexOf(".")
      if (ix > 0) name.substring(0, ix) else ""
    }
  }

  val moduleLocations = List(StandardLocation.MODULE_SOURCE_PATH, StandardLocation.UPGRADE_MODULE_PATH, StandardLocation.SYSTEM_MODULES, StandardLocation.MODULE_PATH, StandardLocation.PATCH_MODULE_PATH)
  for {
    moduleLocation <- moduleLocations
    location1 <- listLocationsForModules(fileManager, moduleLocation).asScala
    location <- location1.asScala
  } {
    val moduleName = inferModuleName(fileManager, location)
    val paths = getLocationAsPaths(fileManager, location).asScala
    val useNioPath = !util.EnumSet.of(StandardLocation.MODULE_PATH, StandardLocation.PATCH_MODULE_PATH).contains(moduleLocation) && paths.forall(java.nio.file.Files.isDirectory(_))
    if (useNioPath) {
      for (path <- paths) {
        val visitor = new SimpleFileVisitor[Path] {
          override def preVisitDirectory(dir: Path, attrs: BasicFileAttributes): FileVisitResult = {
            if (dir.getFileName.toString.contains("-")) FileVisitResult.SKIP_SUBTREE
            else {
              val packageName = path.relativize(dir).toString.replaceAll("/", ".")
              val entry = packageIndex.getOrElseUpdate(packageName, new JpmsPackageEntry(packageName))
              entry.locations.put(location, moduleLocation)
              super.preVisitDirectory(dir, attrs)
            }
          }
        }
        Files.walkFileTree(path, visitor)
      }
    } else {
      for (jfo <- fileManager.list(location, "", java.util.EnumSet.of(JavaFileObject.Kind.CLASS), true).asScala) {
        val binaryName = fileManager.inferBinaryName(location, jfo)
        val (fullPackageName, className) = separatePkgAndClassNames(binaryName)
        for (prefix <- fullPackageName.split('.').inits) {
          val packageName = prefix.mkString(".")
          val entry = packageIndex.getOrElseUpdate(packageName, new JpmsPackageEntry(packageName))
          entry.locations.put(location, moduleLocation)
        }
      }
    }
  }
  val platform = fileManager.list(StandardLocation.CLASS_PATH, "", java.util.EnumSet.of(JavaFileObject.Kind.CLASS), true)
  platform.iterator().forEachRemaining { jfo: JavaFileObject =>
    val binaryName = fileManager.inferBinaryName(StandardLocation.CLASS_PATH, jfo)
    val (fullPackageName, simpleClassName) = PackageNameUtils.separatePkgAndClassNames(binaryName.replaceAll("/", "."))
    val moduleName = ""
    for (prefix <- fullPackageName.split('.').inits) {
      val packageName = prefix.mkString(".")
      val entry = packageIndex.getOrElseUpdate(packageName, new JpmsPackageEntry(packageName))
      entry.locations.put(StandardLocation.CLASS_PATH, StandardLocation.CLASS_PATH)
    }
  }

  private[nsc] def hasPackage(pkg: String): Boolean = {
    packageIndex.contains(pkg)
  }
  private[nsc] def packages(inPackage: String): Seq[PackageEntry] = {
    packageIndex.values.iterator.filter(x => x.parentPackage == inPackage).toArray[PackageEntry]
  }

  private[nsc] def classes(inPackage: String): Seq[ClassFileEntry] = {
    packageIndex.get(inPackage) match {
      case None => Nil
      case Some(entry) =>
        entry.locations.asScala.iterator.flatMap[ClassFileEntry] {
          case (location: Location, outerLocation: StandardLocation) =>
            if (isModuleOrientedLocation(outerLocation)) {
              val moduleName = inferModuleName(fileManager, location)
              for {
                // TODO JPMS We don't have the resolved module graph here to help us out. Is that a problem?
                // if (impl.hasModule(moduleName))
                jfo <- fileManager.list(location, inPackage, util.EnumSet.of(JavaFileObject.Kind.CLASS), false).asScala
                path = asPath(fileManager, jfo)
                if (!path.getFileName.toString.contains("-"))
              } yield {
                JpmsClassFileEntryImpl(new PlainNioFile(path), moduleName, outerLocation)
              }
            } else {
              val listing = fileManager.list(outerLocation, inPackage, util.EnumSet.of(JavaFileObject.Kind.CLASS), false).asScala
              listing.iterator.map { jfo =>
                val moduleName = ""
                JpmsClassFileEntryImpl(new PlainNioFile(asPath(fileManager, jfo)), moduleName, outerLocation)
              }
            }
        }.toArray[ClassFileEntry]
    }
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
