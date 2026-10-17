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

import scala.collection.{immutable, mutable}
import scala.jdk.CollectionConverters._
import scala.reflect.io.AbstractFile

/** The JPMS module structure seen by a single compilation run.
 *
 *  Modules are identified by name. The unnamed module, which contains the class path and any sources not
 *  assigned to a named module, is the empty string. The module of a class loaded from the class or module
 *  path is a property of its class path entry (`ClassRepresentation.jpmsModuleName`) rather than of the run.
 */
trait ModuleTable {
  /** The module of a source file being compiled in this run. */
  def moduleOfSource(file: AbstractFile): String

  /** Can code in module `from` access the public types of package `pkg` in module `target`?
   *  True if `from` and `target` are the same module, or if `from` reads `target` and `target`
   *  exports `pkg` to `from`.
   */
  def isPackageAccessible(from: String, target: String, pkg: String): Boolean

  /** Why `pkg` of `target` is not accessible from `from`, for an error message. */
  def explainInaccessible(from: String, target: String, pkg: String): String = ""
}

object ModuleTable {
  /** No module system: everything is in the unnamed module and every package is accessible. */
  object Unrestricted extends ModuleTable {
    def moduleOfSource(file: AbstractFile): String = ""
    def isPackageAccessible(from: String, target: String, pkg: String): Boolean = true
  }
}

/** Resolves the module graph for a run.
 *
 *  Implemented by class paths that know about modules. Resolution has to wait until the parser has seen
 *  any `module-info.java` among the sources, and is repeated for every run, as the declaration may change.
 */
trait ModuleResolver {
  /** @param sourceModule the module declared by a `module-info.java` compiled in this run, if any */
  def resolve(sourceModule: Option[java.lang.module.ModuleDescriptor]): ModuleTable
}

/** A module table backed by a resolved module graph. */
final class GraphModuleTable(graph: ResolvedModuleGraph) extends ModuleTable {
  private[this] val sourceModules = mutable.HashMap[AbstractFile, String]()
  def moduleOfSource(file: AbstractFile): String =
    sourceModules.getOrElseUpdate(file, graph.moduleForSourceFile(Option(file.file).map(_.toPath.toAbsolutePath.normalize).orNull))

  // From a given module's perspective, the packages it can access in each module it reads. `None` means all packages.
  private[this] val accessiblePackages = mutable.HashMap[String, Map[String, Option[immutable.Set[String]]]]()
  private def computeAccessibility(from: String): Map[String, Option[immutable.Set[String]]] =
    graph.accessibleModulePackages(from).asScala.iterator.map {
      case (target, packages) =>
        val packagesOrAll =
          if (packages eq ResolvedModuleGraph.EXPORT_ALL) None
          else Some(packages.asScala.toSet)
        (target, packagesOrAll)
    }.toMap

  override def explainInaccessible(from: String, target: String, pkg: String): String =
    graph.explainInaccessible(from, target, pkg)

  def isPackageAccessible(from: String, target: String, pkg: String): Boolean =
    from == target || {
      // Classes on the class path have the empty module name, the graph calls the unnamed module UNNAMED_MODULE_NAME
      val target1 = if (target.isEmpty) ModuleGraphResolver.UNNAMED_MODULE_NAME else target
      accessiblePackages.getOrElseUpdate(from, computeAccessibility(from)).get(target1) match {
        case Some(None)      => true
        case Some(Some(set)) => set(pkg)
        case None            => false
      }
    }
}
