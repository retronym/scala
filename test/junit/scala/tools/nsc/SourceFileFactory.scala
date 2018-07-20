package scala.tools.nsc

import java.io.Closeable
import java.nio.file.{Files, Path}

import scala.reflect.internal.util.{BatchSourceFile, SourceFile}
import scala.reflect.io.PlainNioFile
import scala.tools.nsc

final class SourceFileFactory(val baseDirectory: Path) extends Closeable {
  def this() { this(Files.createTempDirectory("test-")) }
  def apply(code: String): SourceFile = apply("a.scala", code)
  def apply(path: String, code: String): SourceFile = {
    val f = baseDirectory.resolve(path)
    Files.createDirectories(f.getParent)
    f.toFile.deleteOnExit()
    val content = code.getBytes()
    Files.write(f, content)
    new BatchSourceFile(nsc.io.AbstractFile.getFile(f.toFile))
  }
  def delete(): Unit = {
    new PlainNioFile(baseDirectory).delete()
  }
  override def close(): Unit = delete()
}
