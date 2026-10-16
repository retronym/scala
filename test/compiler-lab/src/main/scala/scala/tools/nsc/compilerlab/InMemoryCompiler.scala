package scala.tools.nsc.compilerlab

import scala.reflect.internal.Reporter.ERROR
import scala.reflect.io.VirtualDirectory
import scala.tools.nsc.{Global, Settings}
import scala.tools.nsc.reporters.StoreReporter

/** Compiles one corpus, again and again, with no file I/O after construction.
 *
 *  Sources are in-memory files read up front, the classpath is an `InMemoryClasspath` snapshot installed as the
 *  platform's classpath (bypassing `PathResolver`), and output goes to an in-memory directory: class files are
 *  generated and serialized, just not written.
 *
 *  By default each compilation starts from scratch, with a new `Global`: it pays for everything a cold compile does
 *  (definitions, classpath indexing, symbol loading) except reading files and JIT warmup, and nothing survives from
 *  one compilation to the next. With `resident = true`, compilations share one `Global`, each a new `Run`, as a
 *  compile server or the REPL does.
 */
final class InMemoryCompiler(val corpus: Corpus, extraOptions: List[String] = Nil, resident: Boolean = false) {
  /** The classpath snapshot this compiler reads. */
  val classpathSnapshot: InMemoryClasspath = InMemoryClasspath.forCorpus(corpus)

  /** The output of the latest compilation. */
  var output: VirtualDirectory = newOutput()

  private def newOutput() = new VirtualDirectory("compiler-lab-output", None)

  private def newGlobal(out: VirtualDirectory): Global = {
    val args = corpus.options ::: extraOptions
    val settings = new Settings(msg => throw new IllegalArgumentException(s"${corpus.name}: $msg"))
    val (ok, residual) = settings.processArguments(args, processAll = true)
    if (!ok || residual.nonEmpty) throw new IllegalArgumentException(s"bad options: ${args.mkString(" ")}")
    settings.outputDirs.setSingleOutput(out)
    val cp = classpathSnapshot.classPath(corpus.sourcePath)
    new Global(settings, new StoreReporter(settings)) {
      override lazy val platform: ThisPlatform = new GlobalPlatform {
        currentClassPath = Some(cp)
      }
    }
  }

  private lazy val residentGlobal: Global = newGlobal(output)

  /** One full compilation of the corpus. */
  def compile(): Unit = {
    val global =
      if (resident) residentGlobal
      else {
        output = newOutput()
        newGlobal(output)
      }
    val reporter = global.reporter.asInstanceOf[StoreReporter]
    reporter.reset()
    try new global.Run().compileFiles(corpus.files)
    catch {
      case e: Exception =>
        throw new IllegalStateException(s"${corpus.name}: compiler crashed after:\n${errors(reporter).mkString("\n")}", e)
    } finally if (!resident) global.close()
    if (reporter.hasErrors) throw new IllegalStateException(s"${corpus.name} failed to compile:\n${errors(reporter).mkString("\n")}")
  }

  private def errors(r: StoreReporter) = r.infos.iterator.filter(_.severity == ERROR).take(10).map(i => s"${i.pos}: ${i.msg}").toList

  /** Number of files in the latest output, to check the backend ran. */
  def outputCount: Int = {
    def count(f: scala.reflect.io.AbstractFile): Int = if (f.isDirectory) f.iterator.map(count).sum else 1
    count(output)
  }
}
