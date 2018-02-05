/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 */

package scala
package reflect
package io

import java.lang.reflect.Field
import java.nio.file.spi.FileSystemProvider
import java.util.concurrent.atomic.AtomicInteger

import com.github.marschall.memoryfilesystem.{MemoryFileSystemBuilder, MemoryFileSystemProvider}

import scala.collection.JavaConverters.asScalaIteratorConverter
import scala.collection.mutable

/**
 * An in-memory directory.
 *
 * @author Lex Spoon
 *
 * ''Note:  This library is considered experimental and should not be used unless you know what you are doing.''
 */
class VirtualDirectory(val name: String, maybeContainer: Option[VirtualDirectory])
extends AbstractFile {
  def path: String =
    maybeContainer match {
      case None => name
      case Some(parent) => parent.path+'/'+ name
    }

  def absolute = this

  def container = maybeContainer.get
  def isDirectory = true
  override def isVirtual = true
  val lastModified: Long = System.currentTimeMillis

  override def file = null
  override def input = sys.error("directories cannot be read")
  override def output = sys.error("directories cannot be written")

  /** Does this abstract file denote an existing file? */
  def create() { unsupported() }

  /** Delete the underlying file or directory (recursively). */
  def delete() { unsupported() }

  /** Returns an abstract file with the given name. It does not
   *  check that it exists.
   */
  def lookupNameUnchecked(name: String, directory: Boolean): AbstractFile = unsupported()

  private val files = mutable.Map.empty[String, AbstractFile]

  // the toList is so that the directory may continue to be
  // modified while its elements are iterated
  def iterator = files.values.toList.iterator

  override def lookupName(name: String, directory: Boolean): AbstractFile =
    (files get name filter (_.isDirectory == directory)).orNull

  override def fileNamed(name: String): AbstractFile =
    Option(lookupName(name, directory = false)) getOrElse {
      val newFile = new VirtualFile(name, path+'/'+name)
      files(name) = newFile
      newFile
    }

  override def subdirectoryNamed(name: String): AbstractFile =
    Option(lookupName(name, directory = true)) getOrElse {
      val dir = new VirtualDirectory(name, Some(this))
      files(name) = dir
      dir
    }

  def clear() {
    files.clear()
  }
}

object VirtualDirectory {
  private val id = new AtomicInteger()
  private lazy val provider: FileSystemProvider = java.nio.file.spi.FileSystemProvider.installedProviders().iterator().asScala.find(_.getScheme == "memory").orNull

  /** Creates a named virtual filesystem. The caller must close the filesystem to release resources. */
  def newNioVirtualDirectory(namePrefix: String): (String, java.nio.file.FileSystem) = {
    val name = namePrefix + "$" + id.incrementAndGet()
    (name, MemoryFileSystemBuilder.newEmpty().build(name))
  }

  private lazy val MemoryFileSystem_fileSystems: Field = {
    val field = classOf[MemoryFileSystemProvider].getDeclaredField("fileSystems")
    field.setAccessible(true)
    field
  }

  /** Creates a named virtual filesystem. The caller need not close this filesystem to release resources. Conversion from URIs to Paths in this filesystem is not supported. */
  def newAnonymousNioVirtualDirectory(): java.nio.file.FileSystem = {
    val (name, fs) = newNioVirtualDirectory("(memory)")
    fs.provider().asInstanceOf[MemoryFileSystemProvider]
    // TODO this is a workaround for https://github.com/marschall/memoryfilesystem/issues/95
    //      maybe jimfs is a better option (pity that is drags in Guava transitively :( )
    val map = MemoryFileSystem_fileSystems.get(fs.provider()).asInstanceOf[java.util.Map[String, _]]
    map.remove(name)
    fs
  }
}