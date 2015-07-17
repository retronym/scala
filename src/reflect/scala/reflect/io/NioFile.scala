package scala.reflect
package io

import java.io.{File => JIOFile, IOException, InputStream, OutputStream}
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{Path => JPath, FileVisitor, FileVisitResult, SimpleFileVisitor, Files}
import scala.collection.JavaConverters._

final class NioFile(nioPath: JPath) extends AbstractFile {
  private def mapPath(f: JPath => JPath) = {
    val newPath = f(nioPath)
    if (newPath eq nioPath) this else new NioFile(newPath)
  }
  override def name: String = nioPath.getFileName.toString

  /** returns an input stream so the file can be read */
  override def input: InputStream = Files.newInputStream(nioPath)

  /** Returns the underlying File if any and null otherwise. */
  override def file: JIOFile = nioPath.toFile

  /** The absolute file, if this is a relative file. */
  override def absolute: AbstractFile = mapPath(_.toAbsolutePath)

  /** Returns an abstract file with the given name. It does not
    * check that it exists.
    */
  override def lookupNameUnchecked(name: String, directory: Boolean): AbstractFile = mapPath(_.resolve(name))

  /** Returns the time that this abstract file was last modified. */
  override def lastModified: Long = Files.getLastModifiedTime(nioPath).toMillis

  /** Returns the containing directory of this abstract file */
  override def container: AbstractFile = mapPath(_.getParent)

  /** Delete the underlying file or directory (recursively). */
  override def delete(): Unit = {
    Files.walkFileTree(nioPath, NioFile.deleteVisitor)
  }

  override def isDirectory: Boolean = Files.isDirectory(nioPath)

  override def iterator: Iterator[AbstractFile] = Files.list(nioPath).iterator().asScala.map(new NioFile(_))

  override def output: OutputStream = Files.newOutputStream(nioPath)

  override def create(): Unit =
    if (!Files.exists(nioPath)) Files.createFile(nioPath)
    // or maybe try { createFile(..., CREATE_NEW) } catch { case _: FileAlreadyExistsException =>  }

  override def path: String = nioPath.toAbsolutePath.toString

  override def lookupName(name: String, directory: Boolean): AbstractFile = {
    val resolved = nioPath.resolve(name)
    if (!Files.exists(resolved)) null
    else {
      assert(Files.isDirectory(resolved) == directory, s"$resolved is not a ${if (directory) "directory" else "file"}")
      new NioFile(resolved)
    }
  }
}

object NioFile {
  private val deleteVisitor: FileVisitor[JPath] = new SimpleFileVisitor[JPath] {
    override def visitFile(file: JPath, attrs: BasicFileAttributes): FileVisitResult = {
      Files.delete(file)
      super.visitFile(file, attrs)
    }

    override def postVisitDirectory(dir: JPath, exc: IOException): FileVisitResult = {
      Files.delete(dir)
      FileVisitResult.CONTINUE
    }
  }

}