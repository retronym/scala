/* NSC -- new Scala compiler
 * Copyright 2006-2013 LAMP/EPFL
 * @author  Martin Odersky
 */


package scala.tools.nsc
package util

import java.net.URL
import scala.collection.{ mutable, immutable }
import io.{ File, Directory, Path, Jar, AbstractFile }
import scala.reflect.internal.util.StringOps.splitWhere
import Jar.isJarOrZip
import File.pathSeparator
import java.net.MalformedURLException
import java.util.regex.PatternSyntaxException
import scala.reflect.runtime.ReflectionUtils
import ClassPath._
import scala.reflect.io.FileZipArchive
import scala.tools.nsc.classpath.ZipFileIndexClasspath
import scala.tools.nsc.classpath.FlatClasspath

/** <p>
 *    This module provides star expansion of '-classpath' option arguments, behaves the same as
 *    java, see [http://java.sun.com/javase/6/docs/technotes/tools/windows/classpath.html]
 *  </p>
 *
 *  @author Stepan Koltsov
 */
object ClassPath {
  import scala.language.postfixOps

  /** Expand single path entry */
  private def expandS(pattern: String): List[String] = {
    val wildSuffix = File.separator + "*"

    /* Get all subdirectories, jars, zips out of a directory. */
    def lsDir(dir: Directory, filt: String => Boolean = _ => true) =
      dir.list filter (x => filt(x.name) && (x.isDirectory || isJarOrZip(x))) map (_.path) toList

    if (pattern == "*") lsDir(Directory("."))
    else if (pattern endsWith wildSuffix) lsDir(Directory(pattern dropRight 2))
    else if (pattern contains '*') {
      try {
        val regexp = ("^" + pattern.replaceAllLiterally("""\*""", """.*""") + "$").r
        lsDir(Directory(pattern).parent, regexp findFirstIn _ isDefined)
      }
      catch { case _: PatternSyntaxException => List(pattern) }
    }
    else List(pattern)
  }

  /** Split classpath using platform-dependent path separator */
  def split(path: String): List[String] = (path split pathSeparator).toList filterNot (_ == "") distinct

  /** Join classpath using platform-dependent path separator */
  def join(paths: String*): String  = paths filterNot (_ == "") mkString pathSeparator

  /** Split the classpath, apply a transformation function, and reassemble it. */
  def map(cp: String, f: String => String): String = join(split(cp) map f: _*)

  /** Expand path and possibly expanding stars */
  def expandPath(path: String, expandStar: Boolean = true): List[String] =
    if (expandStar) split(path) flatMap expandS
    else split(path)

  /** Expand dir out to contents, a la extdir */
  def expandDir(extdir: String): List[String] = {
    AbstractFile getDirectory extdir match {
      case null => Nil
      case dir  => dir filter (_.isClassContainer) map (x => new java.io.File(dir.file, x.name) getPath) toList
    }
  }
  /** Expand manifest jar classpath entries: these are either urls, or paths
   *  relative to the location of the jar.
   */
  def expandManifestPath(jarPath: String): List[URL] = {
    val file = File(jarPath)
    if (!file.isFile) return Nil

    val baseDir = file.parent
    new Jar(file).classPathElements map (elem =>
      specToURL(elem) getOrElse (baseDir / elem).toURL
    )
  }

  def specToURL(spec: String): Option[URL] =
    try Some(new URL(spec))
    catch { case _: MalformedURLException => None }

  /** A class modeling aspects of a ClassPath which should be
   *  propagated to any classpaths it creates.
   */
  abstract class ClassPathContext[T] extends classpath.ClasspathFactory[ClassPath[T]] {
    def expandPath(path: String, expandStar: Boolean = true): List[String] = ClassPath.this.expandPath(path, expandStar)
    def expandDir(extdir: String): List[String] = ClassPath.this.expandDir(extdir)
    /** A filter which can be used to exclude entities from the classpath
     *  based on their name.
     */
    def isValidName(name: String): Boolean = true

    /** Filters for assessing validity of various entities.
     */
    def validClassFile(name: String)  = endsClass(name) && isValidName(name)
    def validPackage(name: String)    = (name != "META-INF") && (name != "") && (name.charAt(0) != '.')
    def validSourceFile(name: String) = endsScala(name) || endsJava(name)

    /** From the representation to its identifier.
     */
    def toBinaryName(rep: T): String

    def sourcesInPath(path: String): List[ClassPath[T]] =
      for (file <- expandPath(path, expandStar = false) ; dir <- Option(AbstractFile getDirectory file)) yield
        new SourcePath[T](dir, this)
  }

  class JavaContext extends ClassPathContext[AbstractFile] {
    def toBinaryName(rep: AbstractFile) = {
      val name = rep.name
      assert(endsClass(name), name)
      name.substring(0, name.length - 6)
    }
    def newClassPath(dir: AbstractFile): ClassPath[AbstractFile] = {
        new DirectoryClassPath(dir, this)
    }
  }

  object DefaultJavaContext extends JavaContext

  private def endsClass(s: String) = s.length > 6 && s.substring(s.length - 6) == ".class"
  private def endsScala(s: String) = s.length > 6 && s.substring(s.length - 6) == ".scala"
  private def endsJava(s: String)  = s.length > 5 && s.substring(s.length - 5) == ".java"

  /** From the source file to its identifier.
   */
  def toSourceName(f: AbstractFile): String = {
    val name = f.name

    if (endsScala(name)) name.substring(0, name.length - 6)
    else if (endsJava(name)) name.substring(0, name.length - 5)
    else throw new FatalError("Unexpected source file ending: " + name)
  }
}

/**
 * Represents a package which contains classes and other packages
 */
abstract class ClassPath[T] extends ClassfileLookup {
  type AnyClassRep = ClassPath[T]#ClassRep

  /**
   * The short name of the package (without prefix)
   */
  def name: String

  /**
   * A String representing the origin of this classpath element, if known.
   * For example, the path of the directory or jar.
   */
  def origin: Option[String] = None

  /** A list of URLs representing this classpath.
   */
  def asURLs: List[URL]

  /** The whole classpath in the form of one String.
   */
  def asClasspathString: String

  /** Info which should be propagated to any sub-classpaths.
   */
  def context: ClassPathContext[T]

  /** Lists of entities.
   */
  def classes: IndexedSeq[AnyClassRep]
  def packages: IndexedSeq[ClassPath[T]]
  def sourcepaths: IndexedSeq[AbstractFile]

  /**
   * Represents classes which can be loaded with a ClassfileLoader and/or SourcefileLoader.
   */
  case class ClassRep(binary: Option[T], source: Option[AbstractFile]) {
    def name: String = binary match {
      case Some(x)  => context.toBinaryName(x)
      case _        =>
        assert(source.isDefined)
        toSourceName(source.get)
    }
  }

  /** Filters for assessing validity of various entities.
   */
  def validClassFile(name: String)  = context.validClassFile(name)
  def validPackage(name: String)    = context.validPackage(name)
  def validSourceFile(name: String) = context.validSourceFile(name)

  /**
   * Find a ClassRep given a class name of the form "package.subpackage.ClassName".
   * Does not support nested classes on .NET
   */
  def findClass(name: String): Option[AnyClassRep] =
    splitWhere(name, _ == '.', doDropIndex = true) match {
      case Some((pkgName, rest)) =>
        val pkg = packages find (_.name == pkgName)
        val rep = pkg flatMap (_ findClass rest)
        rep map {
          case x: ClassRep  => x
          case x            => throw new FatalError("Unexpected ClassRep '%s' found searching for name '%s'".format(x, name))
        }
      case _ =>
        classes find (_.name == name)
    }

  def findClassFile(name: String): Option[AbstractFile] =
    findClass(name) match {
      case Some(ClassRep(Some(x: AbstractFile), _)) => Some(x)
      case _                                        => None
    }

  def sortString = join(split(asClasspathString).sorted: _*)
  override def equals(that: Any) = that match {
    case x: ClassPath[_]  => this.sortString == x.sortString
    case _                => false
  }
  override def hashCode = sortString.hashCode()
}

/**
 * A Classpath containing source files
 */
class SourcePath[T](dir: AbstractFile, val context: ClassPathContext[T]) extends ClassPath[T] {
  def name = dir.name
  override def origin = dir.underlyingSource map (_.path)
  def asURLs = if (dir.file == null) Nil else List(dir.toURL)
  def asClasspathString = dir.path
  val sourcepaths: IndexedSeq[AbstractFile] = IndexedSeq(dir)

  private def traverse() = {
    val classBuf   = immutable.Vector.newBuilder[ClassRep]
    val packageBuf = immutable.Vector.newBuilder[SourcePath[T]]
    dir foreach { f =>
      if (!f.isDirectory && validSourceFile(f.name))
        classBuf += ClassRep(None, Some(f))
      else if (f.isDirectory && validPackage(f.name))
        packageBuf += new SourcePath[T](f, context)
    }
    (packageBuf.result(), classBuf.result())
  }

  lazy val (packages, classes) = traverse()
  override def toString() = "sourcepath: "+ dir.toString()
}

/**
 * A directory (or a .jar file) containing classfiles and packages
 */
class DirectoryClassPath(val dir: AbstractFile, val context: ClassPathContext[AbstractFile]) extends ClassPath[AbstractFile] {
  def name = dir.name
  override def origin = dir.underlyingSource map (_.path)
  def asURLs = if (dir.file == null) List(new URL(name)) else List(dir.toURL)
  def asClasspathString = dir.path
  val sourcepaths: IndexedSeq[AbstractFile] = IndexedSeq()

  // calculates (packages, classes) in one traversal.
  private def traverse() = {
    val classBuf   = immutable.Vector.newBuilder[ClassRep]
    val packageBuf = immutable.Vector.newBuilder[DirectoryClassPath]
    dir foreach {
      f =>
        // Optimization: We assume the file was not changed since `dir` called
        // `Path.apply` and categorized existent files as `Directory`
        // or `File`.
        val isDirectory = f match {
          case pf: io.PlainFile => pf.givenPath match {
            case _: io.Directory => true
            case _: io.File      => false
            case _               => f.isDirectory
          }
          case _ =>
            f.isDirectory
        }
        if (!isDirectory && validClassFile(f.name))
          classBuf += ClassRep(Some(f), None)
        else if (isDirectory && validPackage(f.name))
          packageBuf += new DirectoryClassPath(f, context)
    }
    (packageBuf.result(), classBuf.result())
  }

  lazy val (packages, classes) = traverse()
  override def toString() = "directory classpath: "+ origin.getOrElse("?")
}

class DeltaClassPath[T](original: MergedClassPath[T], subst: Map[ClassPath[T], ClassPath[T]])
extends MergedClassPath[T](original.entries map (e => subst getOrElse (e, e)), original.context) {
  // not sure we should require that here. Commented out for now.
  // require(subst.keySet subsetOf original.entries.toSet)
  // We might add specialized operations for computing classes packages here. Not sure it's worth it.
}

/**
 * A classpath unifying multiple class- and sourcepath entries.
 */
class MergedClassPath[T](
  val entries: IndexedSeq[ClassPath[T]],
  val context: ClassPathContext[T])
extends ClassPath[T] {
  def this(entries: TraversableOnce[ClassPath[T]], context: ClassPathContext[T]) =
    this(entries.toIndexedSeq, context)

  def name = entries.head.name
  def asURLs = (entries flatMap (_.asURLs)).toList
  lazy val sourcepaths: IndexedSeq[AbstractFile] = entries flatMap (_.sourcepaths)

  override def origin = Some(entries map (x => x.origin getOrElse x.name) mkString ("Merged(", ", ", ")"))
  override def asClasspathString: String = join(entries map (_.asClasspathString) : _*)

  lazy val classes: IndexedSeq[AnyClassRep] = {
    var count   = 0
    val indices = mutable.HashMap[String, Int]()
    val cls     = new mutable.ArrayBuffer[AnyClassRep](1024)

    for (e <- entries; c <- e.classes) {
      val name = c.name
      if (indices contains name) {
        val idx      = indices(name)
        val existing = cls(idx)

        if (existing.binary.isEmpty && c.binary.isDefined)
          cls(idx) = existing.copy(binary = c.binary)
        if (existing.source.isEmpty && c.source.isDefined)
          cls(idx) = existing.copy(source = c.source)
      }
      else {
        indices(name) = count
        cls += c
        count += 1
      }
    }
    cls.toIndexedSeq
  }

  lazy val packages: IndexedSeq[ClassPath[T]] = {
    var count   = 0
    val indices = mutable.HashMap[String, Int]()
    val pkg     = new mutable.ArrayBuffer[ClassPath[T]](256)

    for (e <- entries; p <- e.packages) {
      val name = p.name
      if (indices contains name) {
        val idx  = indices(name)
        pkg(idx) = addPackage(pkg(idx), p)
      }
      else {
        indices(name) = count
        pkg += p
        count += 1
      }
    }
    pkg.toIndexedSeq
  }

  private def addPackage(to: ClassPath[T], pkg: ClassPath[T]) = {
    val newEntries: IndexedSeq[ClassPath[T]] = to match {
      case cp: MergedClassPath[_] => cp.entries :+ pkg
      case _                      => IndexedSeq(to, pkg)
    }
    new MergedClassPath[T](newEntries, context)
  }
  def show() {
    println("ClassPath %s has %d entries and results in:\n".format(name, entries.size))
    asClasspathString split ':' foreach (x => println("  " + x))
  }
  override def toString() = "merged classpath "+ entries.mkString("(", "\n", ")")
}

/**
 * The classpath when compiling with target:jvm. Binary files (classfiles) are represented
 * as AbstractFile. nsc.io.ZipArchive is used to view zip/jar archives as directories.
 */
class JavaClassPath(
  containers: IndexedSeq[ClassPath[AbstractFile]],
  context: JavaContext,
  // this is required for sbt-interface compatibility because sbt calls findClass and
  // we have to dispatch it to the correct classpath implementation
  // Yes, I know, it's bat shit insane (GK)
  settings: Settings, flatClasspath: => FlatClasspath)
extends MergedClassPath[AbstractFile](containers, context) {

  // it's a lazy val so if we do not use flat classpath we shouldn't be forcing this
  lazy val cachedFlatClasspath = {
    assert(settings.YclasspathImpl.value == "flat", "Flat classpath has been forced (evaluated) unexpectedly")
    flatClasspath
  }

  override def findClass(name: String): Option[AnyClassRep] = settings.YclasspathImpl.value match {
    case "recursive" => super.findClass(name)
    case "flat" => {
      val classfile = cachedFlatClasspath.findClassFile(name)
      val classRep = classfile.map(file => ClassRep(Some(file), None))
      classRep
    }
  }

}
