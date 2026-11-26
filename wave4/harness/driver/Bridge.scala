// Compiled separately by each scalac 2.13 under comparison and loaded in its own class loader,
// so the driver can hold several compilers in one JVM. Java types only at the boundary.
// Package scala.tools.nsc for access to the platform class path (to put earlier units' classes on it).
package scala.tools.nsc

import java.util.{LinkedHashMap => JMap}
import scala.reflect.internal.util.BatchSourceFile
import scala.reflect.io.{AbstractFile, VirtualDirectory}
import scala.tools.nsc.classpath.{AggregateClassPath, VirtualDirectoryClassPath}
import scala.tools.nsc.reporters.StoreReporter

object W4Bridge {
  /** Compiles the sources with a fresh Global into memory.
   *  Returns Array(errors or null, class files by path). `prior` class files are on the class path.
   *  Errors are "line: message" lines, or one "CRASH <class>: <message> @ <frame>" line. */
  def compile(names: Array[String], srcs: Array[String], flags: Array[String], libJar: String,
              prior: java.util.Map[String, Array[Byte]]): Array[AnyRef] = {
    val s = new Settings(_ => ())
    s.processArguments("-nowarn" :: flags.toList, processAll = true)
    s.usejavacp.value = false
    s.classpath.value = libJar
    val out = new VirtualDirectory("out", None)
    s.outputDirs.setSingleOutput(out)
    val rep = new StoreReporter(s)
    val g = new Global(s, rep)
    try {
      if (!prior.isEmpty) {
        val vd = new VirtualDirectory("prior", None)
        prior.forEach((p, b) => write(vd, p, b))
        g.platform.currentClassPath = Some(AggregateClassPath(List(g.platform.classPath, VirtualDirectoryClassPath(vd))))
      }
      new g.Run().compileSources(names.indices.map(i => new BatchSourceFile(names(i), srcs(i))).toList)
      val errs = rep.infos.filter(_.severity == rep.ERROR).toList
      if (errs.nonEmpty) Array(errs.map(i => i.pos.line + ": " + i.msg).mkString("\n"), null)
      else { val m = new JMap[String, Array[Byte]]; collect(out, "", m); Array(null, m) }
    } catch {
      case t: Throwable =>
        val frame = t.getStackTrace.find(_.getClassName.startsWith("scala.tools")).fold("?")(f => f.getClassName + "." + f.getMethodName)
        Array("CRASH " + t.getClass.getName + ": " + String.valueOf(t.getMessage).linesIterator.take(2).mkString(" | ").take(300) + " @ " + frame, null)
    } finally g.close()
  }
  private def write(root: VirtualDirectory, path: String, bytes: Array[Byte]): Unit = {
    val parts = path.split('/')
    val dir = parts.init.foldLeft(root: AbstractFile)((d, p) => d.subdirectoryNamed(p))
    val o = dir.fileNamed(parts.last).output; try o.write(bytes) finally o.close()
  }
  private def collect(d: AbstractFile, pre: String, m: JMap[String, Array[Byte]]): Unit =
    d.iterator.foreach(f => if (f.isDirectory) collect(f, pre + f.name + "/", m) else m.put(pre + f.name, f.toByteArray))
}
