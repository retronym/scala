//> using scala 2.13.18
//> using dep org.scala-lang:scala-compiler:2.13.18
//> using dep org.openjdk.jol:jol-core:0.17
//> using javaOpt -Xmx12g -Xss16m -Djdk.attach.allowAttachSelf=true

import java.lang.management.ManagementFactory
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, Paths}
import javax.management.ObjectName
import scala.collection.mutable
import scala.jdk.CollectionConverters._
import scala.reflect.internal.util.{BatchSourceFile, Position}
import scala.tools.nsc.{Global, Settings}
import scala.tools.nsc.reporters.ConsoleReporter

/** Survey Tree/Position counts retained after typer, two ways:
 *  - "walk": identity-deduped traversal of unit bodies (incl. TypeTree.original), with each tree's rawatt
 *  - "heap": live class histogram (GC.class_histogram) after typer, all trees on the heap
 *  Prints TSV lines prefixed by `W\t` / `H\t` for the analysis step.
 */
object Survey {
  val corpusRoot = Paths.get(sys.props.getOrElse("survey.corpora", sys.props("user.home") + "/code/scala/compiler-benchmark/corpus"))

  def filter(text: String): String = {
    if (!text.contains("//#")) return text
    val version = "2.13"
    var on = true; var matched = false
    val sb = new java.lang.StringBuilder(text.length)
    for (line <- text.linesWithSeparators) {
      val t = line.trim
      if (t.startsWith("//#")) {
        val words = t.split(" ").filter(_.nonEmpty)
        words(0) match {
          case "//#if" => val m = words.length > 1 && words(1).split('|').exists(version.startsWith); on = m && !matched; if (on) matched = true
          case "//#else" => on = !matched
          case "//#fi" => on = true; matched = false
        }
      } else if (on) sb.append(line)
    }
    sb.toString
  }

  def jar(c: Class[_]) = c.getProtectionDomain.getCodeSource.getLocation.getPath

  def main(args: Array[String]): Unit = {
    val name = args(0)
    val dir = corpusRoot.resolve(name).resolve("latest").toRealPath()
    val paths = Files.walk(dir).iterator.asScala.map(_.toString).filter(p => p.endsWith(".scala") || p.endsWith(".java")).toList.sorted
    val settings = new Settings(sys.error)
    val scalaLib = classOf[scala.Option[_]].getProtectionDomain.getCodeSource.getLocation.getPath
    if (name == "scala") {
      val asm = Files.list(Paths.get(sys.props.getOrElse("survey.deps", sys.props("user.home") + "/code/retronym/jvm-perf-lab/labs/scala2/target/deps"))).iterator.asScala.find(_.toString.contains("scala-asm")).map(_.toString)
      settings.classpath.value = asm.getOrElse(sys.error("scala-asm jar"))
      settings.sourcepath.value = dir.resolve("library").toString
    } else settings.classpath.value = List(scalaLib, jar(classOf[scala.reflect.internal.SymbolTable]), jar(classOf[Global])).mkString(java.io.File.pathSeparator)
    settings.processArgumentString("-Ystop-after:typer -nowarn -d " + Files.createTempDirectory("survey").toString)
    val reporter = new ConsoleReporter(settings)
    val global = new Global(settings, reporter)
    val run = new global.Run
    val sources = paths.map(p => new BatchSourceFile(p, filter(new String(Files.readAllBytes(Paths.get(p)), UTF_8))))
    val t0 = System.nanoTime
    run.compileSources(sources)
    System.err.println(s"compiled ${sources.size} files in ${(System.nanoTime - t0) / 1000000} ms, errors=${reporter.hasErrors}")

    import global._
    val trees = new java.util.IdentityHashMap[Tree, Unit]
    val atts  = new java.util.IdentityHashMap[AnyRef, Unit]
    val treeCls = mutable.Map[String, Long]().withDefaultValue(0L)
    val attCls  = mutable.Map[String, Long]().withDefaultValue(0L)
    val sharedPos = mutable.Map[String, Long]().withDefaultValue(0L) // trees whose rawatt was already seen on another tree
    val walker = new Traverser {
      override def traverse(t: Tree): Unit = if (t != null && !trees.containsKey(t)) {
        trees.put(t, ())
        treeCls(t.getClass.getName) += 1
        val a = t.attachments
        if (atts.containsKey(a)) sharedPos(a.getClass.getName) += 1
        else {
          atts.put(a, ())
          attCls(a.getClass.getName) += 1
          if (!a.isInstanceOf[Position] && !atts.containsKey(a.pos)) { atts.put(a.pos, ()); attCls(a.pos.getClass.getName) += 1 }
        }
        t match {
          case tt: TypeTree if tt.original != null => traverse(tt.original)
          case _ =>
        }
        super.traverse(t)
      }
    }
    // EmptyTree / emptyValDef etc. are singletons; fine to count them once.
    for (u <- run.units) walker.traverse(u.body)
    for ((k, v) <- treeCls) println(s"W\ttree\t$k\t$v")
    for ((k, v) <- attCls) println(s"W\tatt\t$k\t$v")
    for ((k, v) <- sharedPos) println(s"W\tshared\t$k\t$v")

    // Live heap histogram while `run` (and its units) is still reachable.
    val server = ManagementFactory.getPlatformMBeanServer
    val histo = server.invoke(new ObjectName("com.sun.management:type=DiagnosticCommand"), "gcClassHistogram",
      Array[AnyRef](Array.empty[String]), Array("[Ljava.lang.String;")).asInstanceOf[String]
    val Line = """\s*\d+:\s+(\d+)\s+(\d+)\s+(\S+).*""".r
    for (Line(n, b, c) <- histo.linesIterator) println(s"H\t$c\t$n\t$b")
    val rt = Runtime.getRuntime
    System.err.println(s"used heap after typer: ${(rt.totalMemory - rt.freeMemory) >> 20} MB")
    if (run.units.isEmpty) println(global)
  }
}
