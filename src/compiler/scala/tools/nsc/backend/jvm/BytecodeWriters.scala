/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package backend.jvm

import java.io.{DataOutputStream, FileOutputStream, IOException, File => JFile}
import java.nio.file.{FileAlreadyExistsException, Files}
import java.nio.file.attribute.BasicFileAttributes

import scala.tools.nsc.io._
import java.util.jar.Attributes.Name

import scala.language.postfixOps
import scala.reflect.io.PlainNioFile

/** Can't output a file due to the state of the file system. */
class FileConflictException(msg: String, val file: AbstractFile) extends IOException(msg)

/** For the last mile: turning generated bytecode in memory into
 *  something you can use.  Has implementations for writing to class
 *  files, jars, and disassembled/javap output.
 */
trait BytecodeWriters {
  val global: Global
  import global._

  def outputDirectory(sym: Symbol): AbstractFile =
    settings.outputDirs outputDirFor enteringFlatten(sym.sourceFile)

  /**
   * @param clsName cls.getName
   */
  def getFile(base: AbstractFile, clsName: String, suffix: String): AbstractFile = {
    if (base.file != null) {
      fastGetFile(base, clsName, suffix)
    } else {
      def ensureDirectory(dir: AbstractFile): AbstractFile =
        if (dir.isDirectory) dir
        else throw new FileConflictException(s"${base.path}/$clsName$suffix: ${dir.path} is not a directory", dir)
      var dir = base
      val pathParts = clsName.split("[./]").toList
      for (part <- pathParts.init) dir = ensureDirectory(dir) subdirectoryNamed part
      ensureDirectory(dir) fileNamed pathParts.last + suffix
    }
  }
  private def fastGetFile(base: AbstractFile, clsName: String, suffix: String) = {
    val index = clsName.lastIndexOf('/')
    val (packageName, simpleName) = if (index > 0) {
      (clsName.substring(0, index), clsName.substring(index + 1))
    } else ("", clsName)
    val directory = base.file.toPath.resolve(packageName)
//    try {
//      Files.createDirectories(directory)
//    } catch {
//      case _: FileAlreadyExistsException => throw new FileConflictException(s"${base.path}/$clsName$suffix: ${base.path} is not a directory", base)
//    }
    new PlainNioFile(directory.resolve(simpleName + suffix))
  }
  def getFile(sym: Symbol, clsName: String, suffix: String): AbstractFile =
    getFile(outputDirectory(sym), clsName, suffix)

  def factoryNonJarBytecodeWriter(): BytecodeWriter = {
    val emitAsmp  = settings.Ygenasmp.isSetByUser
    val doDump    = settings.Ydumpclasses.isSetByUser
    (emitAsmp, doDump) match {
      case (false, false) => new ClassBytecodeWriter { }
      case (false, true ) => new ClassBytecodeWriter with DumpBytecodeWriter { }
      case (true,  false) => new ClassBytecodeWriter with AsmpBytecodeWriter
      case (true,  true ) => new ClassBytecodeWriter with AsmpBytecodeWriter with DumpBytecodeWriter { }
    }
  }

  trait BytecodeWriter {
    def writeClass(label: String, jclassName: String, jclassBytes: Array[Byte], outfile: AbstractFile): Unit
    def close(): Unit = ()
  }

  class DirectToJarfileWriter(jfile: JFile) extends BytecodeWriter {
    val jarMainAttrs = (
      if (settings.mainClass.isDefault) Nil
      else List(Name.MAIN_CLASS -> settings.mainClass.value)
    )
    val writer = new Jar(jfile).jarWriter(jarMainAttrs: _*)

    def writeClass(label: String, jclassName: String, jclassBytes: Array[Byte], outfile: AbstractFile) {
      assert(outfile == null,
             "The outfile formal param is there just because ClassBytecodeWriter overrides this method and uses it.")
      val path = jclassName + ".class"
      val out  = writer.newOutputStream(path)

      try out.write(jclassBytes, 0, jclassBytes.length)
      finally out.flush()

      informProgress("added " + label + path + " to jar")
    }
    override def close() = writer.close()
  }

  /*
   * The ASM textual representation for bytecode overcomes disadvantages of javap output in three areas:
   *    (a) pickle dingbats undecipherable to the naked eye;
   *    (b) two constant pools, while having identical contents, are displayed differently due to physical layout.
   *    (c) stack maps (classfile version 50 and up) are displayed in encoded form by javap,
   *        their expansion by ASM is more readable.
   *
   * */
  trait AsmpBytecodeWriter extends BytecodeWriter {
    import scala.tools.asm

    private val baseDir = Directory(settings.Ygenasmp.value).createDirectory()

    private def emitAsmp(jclassBytes: Array[Byte], asmpFile: io.File) {
      val pw = asmpFile.printWriter()
      try {
        val cnode = new asm.tree.ClassNode()
        val cr    = new asm.ClassReader(jclassBytes)
        cr.accept(cnode, 0)
        val trace = new scala.tools.asm.util.TraceClassVisitor(new java.io.PrintWriter(new java.io.StringWriter()))
        cnode.accept(trace)
        trace.p.print(pw)
      }
      finally pw.close()
    }

    abstract override def writeClass(label: String, jclassName: String, jclassBytes: Array[Byte], outfile: AbstractFile) {
      super.writeClass(label, jclassName, jclassBytes, outfile)

      val segments = jclassName.split("[./]")
      val asmpFile = segments.foldLeft(baseDir: Path)(_ / _) changeExtension "asmp" toFile;

      asmpFile.parent.createDirectory()
      emitAsmp(jclassBytes, asmpFile)
    }
  }

  trait ClassBytecodeWriter extends BytecodeWriter {
    def writeClass(label: String, jclassName: String, jclassBytes: Array[Byte], outfile: AbstractFile) {
      assert(outfile != null,
             "Precisely this override requires its invoker to hand out a non-null AbstractFile.")
      if (outfile.file != null) {
        try {
          Files.write(outfile.file.toPath, jclassBytes)
        } catch {
          case _: java.nio.file.NoSuchFileException =>
            Files.createDirectories(outfile.file.toPath.getParent)
            Files.write(outfile.file.toPath, jclassBytes)
        }
      } else {
        val outstream = new DataOutputStream(outfile.bufferedOutput)
        try outstream.write(jclassBytes, 0, jclassBytes.length)
        finally outstream.close()
      }

      informProgress("wrote '" + label + "' to " + outfile)
    }
  }

  trait DumpBytecodeWriter extends BytecodeWriter {
    val baseDir = Directory(settings.Ydumpclasses.value).createDirectory()

    abstract override def writeClass(label: String, jclassName: String, jclassBytes: Array[Byte], outfile: AbstractFile) {
      super.writeClass(label, jclassName, jclassBytes, outfile)

      val pathName = jclassName
      val dumpFile = pathName.split("[./]").foldLeft(baseDir: Path) (_ / _) changeExtension "class" toFile;
      dumpFile.parent.createDirectory()
      val outstream = new DataOutputStream(new FileOutputStream(dumpFile.path))

      try outstream.write(jclassBytes, 0, jclassBytes.length)
      finally outstream.close()
    }
  }
}
