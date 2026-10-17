/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc.jpms

import java.io.File.pathSeparator
import java.lang.module.ModuleFinder
import java.net.URL
import java.nio.file.{Path, Paths}

import scala.jdk.CollectionConverters._
import scala.reflect.io.PlainNioFile
import scala.tools.nsc.Settings
import scala.tools.nsc.classpath._
import scala.tools.nsc.io.AbstractFile
import scala.tools.nsc.util.{ClassPath, ClassRepresentation}

/** The class path under `-Yjpms`, built from scalac's own class path implementations.
 *
 *  Each entry is tagged with the module its classes belong to: the system modules by their `jrt:/modules/<name>`
 *  path, module path entries by their (explicit or automatic) module name, and `--patch-module` directories by the
 *  module they patch, ahead of that module's own classes. Class path entries are in the unnamed module.
 *
 *  Lookup is flat: a class is found in the first entry that contains it, whether or not the accessing module
 *  can read the module of that entry. Accessibility is checked afterwards against the module graph, which is
 *  resolved by `java.lang.module.Configuration` for each run.
 */
final class ModulePathClassPath private (underlying: AggregateClassPath, resolver: ModuleGraphResolver) extends ClassPath with ModuleResolver {
  def resolve(sourceModule: Option[JpmsModuleDescriptor]): ModuleTable =
    new GraphModuleTable(resolver.resolve(sourceModule.orNull))

  def entries: Seq[ClassPath] = underlying.aggregates

  private[nsc] def hasPackage(pkg: PackageName): Boolean = underlying.hasPackage(pkg)
  private[nsc] def packages(inPackage: PackageName): Seq[PackageEntry] = underlying.packages(inPackage)
  private[nsc] def classes(inPackage: PackageName): Seq[ClassFileEntry] = underlying.classes(inPackage)
  private[nsc] def sources(inPackage: PackageName): Seq[SourceFileEntry] = underlying.sources(inPackage)
  private[nsc] def list(inPackage: PackageName): ClassPathEntries = underlying.list(inPackage)
  override def findClass(className: String): Option[ClassRepresentation] = underlying.findClass(className)
  def findClassFile(className: String): Option[AbstractFile] = underlying.findClassFile(className)
  def asURLs: Seq[URL] = underlying.asURLs
  def asClassPathStrings: Seq[String] = underlying.asClassPathStrings
  def asSourcePathString: String = underlying.asSourcePathString
  override def toString = s"ModulePathClassPath(${underlying.aggregates.mkString(", ")})"
}

object ModulePathClassPath {
  /** @param containers the ordinary class path of the compilation: the system classes, class path and source path */
  def apply(settings: Settings, containers: Seq[ClassPath], classPathFactory: ClassPathFactory): ModulePathClassPath = {
    def tagged(module: String)(cps: Seq[ClassPath]): Seq[ClassPath] = cps.map(new ModuleTaggedClassPath(_, s"module $module", _ => module))

    if (settings.systemPathValue.isDefined)
      settings.errorFn("-system is not yet supported with -Yjpms")

    val patches = JpmsSettings.patches(settings)
    val patchEntries = patches.toSeq.flatMap { case (module, paths) => paths.flatMap(p => tagged(module)(classPathFactory.classesInPath(p))) }

    // Module path elements are modules (JARs or exploded directories) or directories of modules. Earlier ones win.
    val modulePath = settings.modulePath.value.split(pathSeparator).iterator.filter(_.nonEmpty).map(Paths.get(_)).toList
    val modulePathFinder = ModuleFinder.of(modulePath: _*)
    val moduleEntries = modulePathFinder.findAll().asScala.toSeq.sortBy(_.descriptor.name).flatMap { ref =>
      val location = Paths.get(ref.location.get)
      tagged(ref.descriptor.name)(classPathFactory.classesInPath(location.toString))
    }

    val (system, rest) = containers.partition {
      case _: JrtClassPath | _: FilteringJrtClassPath | _: CtSymClassPath => true
      case _                                                              => false
    }
    val systemEntries = system.map {
      case cp: CtSymClassPath =>
        settings.errorFn("-release is not yet supported with -Yjpms")
        cp
      case cp => new ModuleTaggedClassPath(cp, "system modules", jrtModuleOf)
    }

    val aggregate = AggregateClassPath((patchEntries ++ systemEntries ++ moduleEntries ++ rest).toIndexedSeq)
    val patchRoots: Seq[(Path, String)] = patches.toSeq.flatMap { case (module, paths) => paths.map(p => (Paths.get(p).toAbsolutePath.normalize, module)) }
    val patchModuleOfSource: java.util.function.Function[Path, String] = { path =>
      patchRoots.collectFirst { case (root, module) if path.startsWith(root) => module }.orNull
    }
    val resolver = new ModuleGraphResolver(ModuleFinder.ofSystem(), modulePathFinder, JpmsSettings.classOutput(settings),
      settings.addModules.value.asJava, settings.addExports.value.asJava, settings.addReads.value.asJava, patchModuleOfSource)
    new ModulePathClassPath(aggregate, resolver)
  }

  /** The module of a class in the system image, from its path `/modules/<module>/...` in the `jrt:` file system. */
  private def jrtModuleOf(file: AbstractFile): String = file match {
    case f: PlainNioFile if f.nioPath.getNameCount > 2 && f.nioPath.getName(0).toString == "modules" => f.nioPath.getName(1).toString
    case _                                                                                       => ""
  }
}

/** Tags the classes of `underlying` with their module. Sources are left alone, their module is determined per run. */
private final class ModuleTaggedClassPath(val underlying: ClassPath, description: String, moduleOf: AbstractFile => String) extends ClassPath {
  private def tag(entry: ClassFileEntry): ClassFileEntry = JpmsClassFileEntry(entry.file, moduleOf(entry.file))
  private def tag(entry: ClassRepresentation): ClassRepresentation = entry match {
    case c: ClassFileEntry => tag(c)
    case other             => other
  }

  private[nsc] def hasPackage(pkg: PackageName): Boolean = underlying.hasPackage(pkg)
  private[nsc] def packages(inPackage: PackageName): Seq[PackageEntry] = underlying.packages(inPackage)
  private[nsc] def classes(inPackage: PackageName): Seq[ClassFileEntry] = underlying.classes(inPackage).map(tag(_: ClassFileEntry))
  private[nsc] def sources(inPackage: PackageName): Seq[SourceFileEntry] = underlying.sources(inPackage)
  private[nsc] def list(inPackage: PackageName): ClassPathEntries = {
    val entries = underlying.list(inPackage)
    ClassPathEntries(entries.packages, entries.classesAndSources.map(tag(_: ClassRepresentation)))
  }
  override def findClass(className: String): Option[ClassRepresentation] = underlying.findClass(className).map(tag(_: ClassRepresentation))
  def findClassFile(className: String): Option[AbstractFile] = underlying.findClassFile(className)
  def asURLs: Seq[URL] = underlying.asURLs
  def asClassPathStrings: Seq[String] = underlying.asClassPathStrings
  def asSourcePathString: String = underlying.asSourcePathString
  override def toString = s"$underlying ($description)"
}

private[nsc] final case class JpmsClassFileEntry(file: AbstractFile, override val jpmsModuleName: String) extends ClassFileEntry {
  override def fileName: String = file.name
  override lazy val name: String = FileUtils.stripClassExtension(file.name)
  override def binary: Option[AbstractFile] = Some(file)
  override def source: Option[AbstractFile] = None
}

/** Interpretation of the JPMS settings shared by both class path implementations. */
private[jpms] object JpmsSettings {
  private val EqualsPattern = """(.*)=(.*)""".r

  /** `--patch-module` options: module name to the paths patched into it, in order. */
  def patches(s: Settings): Map[String, List[String]] =
    s.patchModule.value.map {
      case EqualsPattern(module, paths) => (module, paths.split(pathSeparator).toList.filter(_.nonEmpty))
      case arg                          => throw new IllegalArgumentException(s"Unable to parse --patch-module $arg")
    }.groupBy(_._1).view.mapValues(_.flatMap(_._2)).toMap

  def classOutput(s: Settings): ClassOutput = {
    val singleOutput = s.outputDirs.getSingleOutput.get
    if (singleOutput.file != null)
      new ClassOutput.OutputPathClassOutput(singleOutput.file.toPath)
    else
      new ClassOutput.SupplierClassOutput(() => {
        val file = singleOutput.lookupName("module-info.class", directory = false)
        if (file == null) null else file.toByteArray
      })
  }
}
