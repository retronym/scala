/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc
package backend

import java.nio.ByteBuffer

import io.AbstractFile
import scala.tools.nsc.classpath.AggregateClassPath
import scala.tools.nsc.util.ClassPath

/** The platform dependent pieces of Global.
 */
trait Platform {
  val symbolTable: symtab.SymbolTable
  import symbolTable._

  /** The new implementation of compiler classpath. */
  private[nsc] def classPath: ClassPath

  /** Update classpath with a substitution that maps entries to entries */
  def updateClassPath(subst: Map[ClassPath, ClassPath])

  /** Any platform-specific phases. */
  def platformPhases: List[SubComponent]

  /** Symbol for a method which compares two objects. */
  def externalEquals: Symbol

  /** The various ways a boxed primitive might materialize at runtime. */
  def isMaybeBoxed(sym: Symbol): Boolean

  /**
   * Tells whether a class with both a binary and a source representation
   * (found in classpath and in sourcepath) should be re-compiled. Behaves
   * on the JVM similar to javac, i.e. if the source file is newer than the classfile,
   * a re-compile is triggered. On .NET by contrast classfiles always take precedence.
   */
  def needCompile(bin: AbstractFile, src: AbstractFile): Boolean

  /**
    * A class path plugin can modify the classpath before it is used by the compiler.
    *
    * Applications could include:
    *
    *   - Caching the ScalaSignature annotation contents, to avoid the cost of decompressing
    *     and parsing the classfile, akin to the OpenJDK's .sig format for stripped class files.
    *   - Starting a downstream compilation job immediately after the upstream job has completed
    *     the pickler phase ("Build Pipelining")
    */
  abstract class ClassPathPlugin {
    def modifyClassPath(classPath: Seq[ClassPath]): Seq[ClassPath] = classPath
  }

  /** A list of registered classpath plugins */
  private var classPathPlugins: List[ClassPathPlugin] = Nil

  protected final def applyClassPathPlugins(original: ClassPath): ClassPath = {
    val entries = original match {
      case AggregateClassPath(entries) => entries
      case single => single :: Nil
    }
    val entries1 = classPathPlugins.foldLeft(entries) {
      (entries, plugin) => plugin.modifyClassPath(entries)
    }
    AggregateClassPath(entries1)
  }


  /** Registers a new classpath plugin */
  final def addClassPathPlugin(plugin: ClassPathPlugin): Unit = {
    if (!classPathPlugins.contains(plugin))
      classPathPlugins = plugin :: classPathPlugins
  }
}
