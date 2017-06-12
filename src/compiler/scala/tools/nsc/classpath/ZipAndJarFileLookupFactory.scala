/*
 * Copyright (c) 2014 Contributor. All rights reserved.
 */
package scala.tools.nsc.classpath

import java.io.File
import java.net.URL
import java.nio.file.{Files, Path}
import java.nio.file.attribute.BasicFileAttributes

import scala.annotation.tailrec
import scala.reflect.io.{AbstractFile, FileZipArchive, ManifestResources, ZipArchive}
import scala.tools.nsc.util.ClassPath
import scala.tools.nsc.Settings
import FileUtils._
import scala.reflect.internal.util.WeakCache

/**
 * A trait providing an optional cache for classpath entries obtained from zip and jar files.
 * It's possible to create such a cache assuming that entries in such files won't change (at
 * least will be the same each time we'll load classpath during the lifetime of JVM process)
 * - unlike class and source files in directories, which can be modified and recompiled.
 * It allows us to e.g. reduce significantly memory used by PresentationCompilers in Scala IDE
 * when there are a lot of projects having a lot of common dependencies.
 */
sealed trait ZipAndJarFileLookupFactory {
  private val cache = new WeakCache[Path, (Long, Object), ClassPath] {
    override protected def create(k: Path): ClassPath = createForZipFile(AbstractFile.getFile(k.toFile))
    override protected def stamp(k: Path): (Long, Object) =
      (Files.getLastModifiedTime(k).toMillis, Files.readAttributes(k, classOf[BasicFileAttributes]).fileKey())
    override protected def share(v: ClassPath): Boolean = v match {
      case z: ZipArchiveFileLookup[_] => z.addReference()
      case _ => true
    }
  }

  def create(zipFile: AbstractFile, settings: Settings): ClassPath = {
    if (settings.YdisableFlatCpCaching) createForZipFile(zipFile)
    else {
      val (isNewInstance, classpath) = cache.get(zipFile.file.toPath)
      if (isNewInstance && settings.verbose || settings.Ylogcp)
        println(s"$zipFile was not yet in the classpath cache")
      classpath
    }
  }

  protected def createForZipFile(zipFile: AbstractFile): ClassPath
}

/**
 * Manages creation of classpath for class files placed in zip and jar files.
 * It should be the only way of creating them as it provides caching.
 */
object ZipAndJarClassPathFactory extends ZipAndJarFileLookupFactory {
  private case class ZipArchiveClassPath(zipFile: File)
    extends ZipArchiveFileLookup[ClassFileEntryImpl]
    with NoSourcePaths {

    override def findClassFile(className: String): Option[AbstractFile] = {
      val (pkg, simpleClassName) = PackageNameUtils.separatePkgAndClassNames(className)
      classes(pkg).find(_.name == simpleClassName).map(_.file)
    }

    override private[nsc] def classes(inPackage: String): Seq[ClassFileEntry] = files(inPackage)

    override protected def createFileEntry(file: FileZipArchive#Entry): ClassFileEntryImpl = ClassFileEntryImpl(file)
    override protected def isRequiredFileType(file: AbstractFile): Boolean = file.isClass
  }

  /**
   * This type of classpath is closely related to the support for JSR-223.
   * Its usage can be observed e.g. when running:
   * jrunscript -classpath scala-compiler.jar;scala-reflect.jar;scala-library.jar -l scala
   * with a particularly prepared scala-library.jar. It should have all classes listed in the manifest like e.g. this entry:
   * Name: scala/Function2$mcFJD$sp.class
   */
  private case class ManifestResourcesClassPath(file: ManifestResources) extends ClassPath with NoSourcePaths {
    override def findClassFile(className: String): Option[AbstractFile] = {
      val (pkg, simpleClassName) = PackageNameUtils.separatePkgAndClassNames(className)
      classes(pkg).find(_.name == simpleClassName).map(_.file)
    }

    override def asClassPathStrings: Seq[String] = Seq(file.path)

    override def asURLs: Seq[URL] = file.toURLs()

    import ManifestResourcesClassPath.PackageFileInfo
    import ManifestResourcesClassPath.PackageInfo

    /**
     * A cache mapping package name to abstract file for package directory and subpackages of given package.
     *
     * ManifestResources can iterate through the collections of entries from e.g. remote jar file.
     * We can't just specify the path to the concrete directory etc. so we can't just 'jump' into
     * given package, when it's needed. On the other hand we can iterate over entries to get
     * AbstractFiles, iterate over entries of these files etc.
     *
     * Instead of traversing a tree of AbstractFiles once and caching all entries or traversing each time,
     * when we need subpackages of a given package or its classes, we traverse once and cache only packages.
     * Classes for given package can be then easily loaded when they are needed.
     */
    private lazy val cachedPackages: collection.mutable.HashMap[String, PackageFileInfo] = {
      val packages = collection.mutable.HashMap[String, PackageFileInfo]()

      def getSubpackages(dir: AbstractFile): List[AbstractFile] =
        (for (file <- dir if file.isPackage) yield file)(collection.breakOut)

      @tailrec
      def traverse(packagePrefix: String,
                   filesForPrefix: List[AbstractFile],
                   subpackagesQueue: collection.mutable.Queue[PackageInfo]): Unit = filesForPrefix match {
        case pkgFile :: remainingFiles =>
          val subpackages = getSubpackages(pkgFile)
          val fullPkgName = packagePrefix + pkgFile.name
          packages.put(fullPkgName, PackageFileInfo(pkgFile, subpackages))
          val newPackagePrefix = fullPkgName + "."
          subpackagesQueue.enqueue(PackageInfo(newPackagePrefix, subpackages))
          traverse(packagePrefix, remainingFiles, subpackagesQueue)
        case Nil if subpackagesQueue.nonEmpty =>
          val PackageInfo(packagePrefix, filesForPrefix) = subpackagesQueue.dequeue()
          traverse(packagePrefix, filesForPrefix, subpackagesQueue)
        case _ =>
      }

      val subpackages = getSubpackages(file)
      packages.put(ClassPath.RootPackage, PackageFileInfo(file, subpackages))
      traverse(ClassPath.RootPackage, subpackages, collection.mutable.Queue())
      packages
    }

    override private[nsc] def packages(inPackage: String): Seq[PackageEntry] = cachedPackages.get(inPackage) match {
      case None => Seq.empty
      case Some(PackageFileInfo(_, subpackages)) =>
        val prefix = PackageNameUtils.packagePrefix(inPackage)
        subpackages.map(packageFile => PackageEntryImpl(prefix + packageFile.name))
    }

    override private[nsc] def classes(inPackage: String): Seq[ClassFileEntry] = cachedPackages.get(inPackage) match {
      case None => Seq.empty
      case Some(PackageFileInfo(pkg, _)) =>
        (for (file <- pkg if file.isClass) yield ClassFileEntryImpl(file))(collection.breakOut)
    }

    override private[nsc] def list(inPackage: String): ClassPathEntries = ClassPathEntries(packages(inPackage), classes(inPackage))
  }

  private object ManifestResourcesClassPath {
    case class PackageFileInfo(packageFile: AbstractFile, subpackages: Seq[AbstractFile])
    case class PackageInfo(packageName: String, subpackages: List[AbstractFile])
  }

  override protected def createForZipFile(zipFile: AbstractFile): ClassPath =
    if (zipFile.file == null) createWithoutUnderlyingFile(zipFile)
    else ZipArchiveClassPath(zipFile.file)

  private def createWithoutUnderlyingFile(zipFile: AbstractFile) = zipFile match {
    case manifestRes: ManifestResources =>
      ManifestResourcesClassPath(manifestRes)
    case _ =>
      val errorMsg = s"Abstract files which don't have an underlying file and are not ManifestResources are not supported. There was $zipFile"
      throw new IllegalArgumentException(errorMsg)
  }
}

/**
 * Manages creation of classpath for source files placed in zip and jar files.
 * It should be the only way of creating them as it provides caching.
 */
object ZipAndJarSourcePathFactory extends ZipAndJarFileLookupFactory {
  private case class ZipArchiveSourcePath(zipFile: File)
    extends ZipArchiveFileLookup[SourceFileEntryImpl]
    with NoClassPaths {

    override def asSourcePathString: String = asClassPathString

    override private[nsc] def sources(inPackage: String): Seq[SourceFileEntry] = files(inPackage)

    override protected def createFileEntry(file: FileZipArchive#Entry): SourceFileEntryImpl = SourceFileEntryImpl(file)
    override protected def isRequiredFileType(file: AbstractFile): Boolean = file.isScalaOrJavaSource
  }

  override protected def createForZipFile(zipFile: AbstractFile): ClassPath = ZipArchiveSourcePath(zipFile.file)
}
