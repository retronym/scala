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
  def resolve(sourceModule: Option[JpmsModuleDescriptor]): ModuleTable
}
