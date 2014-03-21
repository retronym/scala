/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.nsc
package backend.jvm

import java.io.{ DataOutputStream, FileOutputStream, IOException, OutputStream, File => JFile }
import scala.tools.nsc.io._
import java.util.jar.Attributes.Name
import scala.language.postfixOps
import scala.util.control.NonFatal

/** Can't output a file due to the state of the file system. */
class FileConflictException(msg: String, val file: AbstractFile) extends IOException(msg)

/**
 * For the last mile: turning generated bytecode in memory into
 *  something you can use.  Has implementations for writing to class
 *  files, jars, and disassembled/javap output.
 */
trait BytecodeWriters {
  val global: Global
  import global._

  def factoryNonJarBytecodeWriter(): BytecodeWriter = {
    type Writer  = ClassBytecodeWriter
    val emitAsmp = settings.Ygenasmp.isSetByUser
    val doDump   = settings.Ydumpclasses.isSetByUser

    (emitAsmp, doDump) match {
      case (false, false) => new Writer
      case (false, true)  => new Writer with DumpBytecodeWriter
      case (true,  false) => new Writer with AsmpBytecodeWriter
      case (true,  true)  => new Writer with AsmpBytecodeWriter with DumpBytecodeWriter
    }
  }

  trait BytecodeWriter {
    type OutputFolder
    def NoOutputFolder: OutputFolder
    def outputFolder(csym: Symbol, cName: String, cunit: CompilationUnit): OutputFolder

    def writeClassToFolderNow(label: String, jclassName: String, jclassBytes: Array[Byte], folder: OutputFolder): Unit

    def writeClass(label: String, jclassName: String, jclass: scala.tools.asm.ClassWriter, sym: Symbol): Unit
    def close(): Unit = ()
  }

  final class DirectToJarfileWriter(jfile: JFile) extends BytecodeWriter {
    type OutputFolder = Null
    def NoOutputFolder = null
    def outputFolder(csym: Symbol, cName: String, cunit: CompilationUnit): OutputFolder = NoOutputFolder

    val jarMainAttrs = (
      if (settings.mainClass.isDefault) Nil
      else List(Name.MAIN_CLASS -> settings.mainClass.value))
    val writer = new Jar(jfile).jarWriter(jarMainAttrs: _*)

    def writeClass(label: String, jclassName: String, jclass: scala.tools.asm.ClassWriter, sym: Symbol): Unit =
      writeClassToFolderNow(label, jclassName, jclass.toByteArray(), NoOutputFolder)

    def writeClassToFolderNow(label: String, jclassName: String, jclassBytes: Array[Byte], ignored: OutputFolder): Unit = {
      val path = jclassName + ".class"
      val out = writer.newOutputStream(path)

      try out.write(jclassBytes, 0, jclassBytes.length)
      finally out.flush()

      informProgress(s"added $label $path to jar")
    }
    override def close() = writer.close()
  }

  class ClassBytecodeWriter extends BytecodeWriter {
    type OutputFolder = AbstractFile
    def NoOutputFolder = null
    final def outputFolder(csym: Symbol, cName: String, cunit: CompilationUnit): OutputFolder =
      try outputDirectory(csym)
      catch {
        case NonFatal(ex) =>
          cunit.error(cunit.body.pos, s"Couldn't create file for class $cName\n${ex.getMessage}")
          NoOutputFolder
      }

    final protected def outputDirectory(sym: Symbol): AbstractFile =
      settings.outputDirs outputDirFor enteringFlatten(sym.sourceFile)

    def writeClass(label: String, jclassName: String, jclass: scala.tools.asm.ClassWriter, sym: Symbol): Unit =
      try writeClassToFolderNow(label, jclassName, jclass.toByteArray(), outputDirectory(sym))
      catch {
        case e: java.lang.RuntimeException if e != null && (e.getMessage contains "too large!") =>
          reporter.error(sym.pos,
            s"Could not write class $jclassName because it exceeds JVM code size limits. ${e.getMessage}")
      }

    /**
     * @param clsName cls.getName
     */
    private def getFile(base: AbstractFile, clsName: String, suffix: String): AbstractFile = {
      def ensureDirectory(dir: AbstractFile): AbstractFile =
        if (dir.isDirectory) dir
        else throw new FileConflictException(s"${base.path}/$clsName$suffix: ${dir.path} is not a directory", dir)
      var dir = base
      val pathParts = clsName.split("[./]").toList
      for (part <- pathParts.init) dir = ensureDirectory(dir) subdirectoryNamed part
      ensureDirectory(dir) fileNamed pathParts.last + suffix
    }

    def writeClassToFolderNow(label: String, jclassName: String, jclassBytes: Array[Byte], folder: OutputFolder): Unit = {
      val outfile: AbstractFile = getFile(folder, jclassName, ".class")

      val outstream = new DataOutputStream(outfile.bufferedOutput)

      try outstream.write(jclassBytes, 0, jclassBytes.length)
      finally outstream.close()
      informProgress("wrote '" + label + "' to " + outfile)
    }
  }

  /*
   * The ASM textual representation for bytecode overcomes disadvantages of javap ouput in three areas:
   *    (a) pickle dingbats undecipherable to the naked eye;
   *    (b) two constant pools, while having identical contents, are displayed differently due to physical layout.
   *    (c) stack maps (classfile version 50 and up) are displayed in encoded form by javap,
   *        their expansion by ASM is more readable.
   *
   * */
  trait AsmpBytecodeWriter extends ClassBytecodeWriter {
    import scala.tools.asm

    private val baseDir = Directory(settings.Ygenasmp.value).createDirectory()

    private def emitAsmp(jclassBytes: Array[Byte], asmpFile: io.File) {
      val pw = asmpFile.printWriter()
      try {
        val cnode = new asm.tree.ClassNode()
        val cr = new asm.ClassReader(jclassBytes)
        cr.accept(cnode, 0)
        val trace = new scala.tools.asm.util.TraceClassVisitor(new java.io.PrintWriter(new java.io.StringWriter()))
        cnode.accept(trace)
        trace.p.print(pw)
      } finally pw.close()
    }

    abstract override def writeClassToFolderNow(label: String, jclassName: String, jclassBytes: Array[Byte], folder: AbstractFile) {
      super.writeClassToFolderNow(label, jclassName, jclassBytes, folder)

      val segments = jclassName.split("[./]")
      val asmpFile = segments.foldLeft(baseDir: Path)(_ / _) changeExtension "asmp" toFile;

      asmpFile.parent.createDirectory()
      emitAsmp(jclassBytes, asmpFile)
    }
  }

  trait DumpBytecodeWriter extends ClassBytecodeWriter {
    val baseDir = Directory(settings.Ydumpclasses.value).createDirectory()

    abstract override def writeClassToFolderNow(label: String, jclassName: String, jclassBytes: Array[Byte], folder: AbstractFile) {
      super.writeClassToFolderNow(label, jclassName, jclassBytes, folder)

      val pathName = jclassName
      val dumpFile = pathName.split("[./]").foldLeft(baseDir: Path)(_ / _) changeExtension "class" toFile;
      dumpFile.parent.createDirectory()
      val outstream = new DataOutputStream(new FileOutputStream(dumpFile.path))

      try outstream.write(jclassBytes, 0, jclassBytes.length)
      finally outstream.close()
    }
  }
}
