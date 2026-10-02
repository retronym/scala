package scala.tools.nsc
package typechecker

import java.nio.file.{Files, Path, Paths}

import scala.jdk.CollectionConverters._
import scala.reflect.internal.util.BatchSourceFile
import scala.tools.nsc.interactive.{Global => IGlobal, Response}
import scala.tools.nsc.reporters.StoreReporter

/** Harness for exploring `-Yrefchecks-after-errors` (scala/scala#11030).
 *
 *  Compiles a snippet twice, without and with the flag, and reports which diagnostics were added (`extra`) or
 *  lost (`lost`). Also captures compiler crashes (the interesting failure mode of running refchecks over trees that
 *  typer left half-typed).
 *
 *  Run the corpus scan against partest `neg` files with:
 *  {{{
 *  sbt "junit/Test/runMain scala.tools.nsc.typechecker.RefchecksAfterErrorsCorpusScan test/files/neg"
 *  }}}
 */
object RefchecksAfterErrorsHarness {
  final case class Diag(severity: String, line: Int, msg: String) {
    // first line only, enough to identify; multi-line explanations are noise for comparison
    def key: (String, Int, String) = (severity, line, msg.linesIterator.nextOption().getOrElse(""))
    override def toString = s"$severity:$line: ${msg.linesIterator.nextOption().getOrElse("")}"
  }
  final case class Outcome(diags: List[Diag], crash: Option[Throwable]) {
    def errors: List[Diag] = diags.filter(_.severity == "error")
    def render: String = diags.mkString("\n") + crash.fold("")(t => s"\nCRASH: $t")
  }
  final case class Comparison(baseline: Outcome, withFlag: Outcome) {
    def extra: List[Diag] = {
      val base = baseline.diags.map(_.key).toSet
      withFlag.diags.filterNot(d => base(d.key))
    }
    def lost: List[Diag] = {
      val now = withFlag.diags.map(_.key).toSet
      baseline.diags.filterNot(d => now(d.key))
    }
    def render: String =
      s"""--- baseline
         |${baseline.render}
         |--- with -Yrefchecks-after-errors
         |${withFlag.render}
         |--- extra
         |${extra.mkString("\n")}""".stripMargin
  }

  val Flag = "-Yrefchecks-after-errors"
  val defaultArgs = List("-usejavacp", "-deprecation", "-feature", "-Xlint")

  def newGlobal(args: List[String]): (Global, StoreReporter) = {
    val settings = new Settings(msg => throw new IllegalArgumentException(msg))
    val (ok, rest) = settings.processArguments(args, processAll = true)
    assert(ok && rest.isEmpty, s"bad args $args / $rest")
    settings.outputDirs.setSingleOutput(new scala.reflect.io.VirtualDirectory("(memory)", None))
    val r = new StoreReporter(settings)
    (new Global(settings, r), r)
  }

  def diagsOf(r: StoreReporter): List[Diag] =
    r.infos.toList.map(i => Diag(i.severity.toString.toLowerCase, if (i.pos.isDefined) i.pos.line else 0, i.msg))

  /** Compile `sources` (name -> code) in one run. */
  def batch(sources: List[(String, String)], flag: Boolean, args: List[String] = Nil): Outcome = {
    val (g, r) = newGlobal(defaultArgs ::: args ::: (if (flag) List(Flag) else Nil))
    runOn(g, r, sources)
  }

  def runOn(g: Global, r: StoreReporter, sources: List[(String, String)]): Outcome = {
    r.reset()
    val run = new g.Run
    val crash =
      try { run.compileSources(sources.map { case (n, c) => new BatchSourceFile(n, c) }); None }
      catch { case t: Throwable => Some(t) }
    Outcome(diagsOf(r), crash)
  }

  def compare(code: String, args: List[String] = Nil): Comparison =
    Comparison(batch(List("t.scala" -> code), flag = false, args), batch(List("t.scala" -> code), flag = true, args))

  /** Presentation compiler variant: the interactive `Global` has its own driver (`backgroundCompile` -> `typeCheck`)
   *  which never goes through `Global.compileUnitsInternal`, so the PR's change is inert there.
   */
  def presentationCompiler(code: String, flag: Boolean): List[Diag] = {
    val settings = new Settings(msg => throw new IllegalArgumentException(msg))
    settings.processArguments(defaultArgs ::: (if (flag) List(Flag) else Nil), processAll = true)
    settings.outputDirs.setSingleOutput(new scala.reflect.io.VirtualDirectory("(memory)", None))
    val r = new StoreReporter(settings)
    val g = new IGlobal(settings, r)
    try {
      val src = new BatchSourceFile("t.scala", code)
      val reload = new Response[Unit]; g.askReload(List(src), reload); reload.get
      val typed = new Response[g.Tree]; g.askLoadedTyped(src, keepLoaded = true, typed); typed.get
      diagsOf(r)
    } finally g.askShutdown()
  }

  /** Corpus scan: single-file `.scala` tests in `dir`, honouring a sibling `.flags` file. */
  def scan(dir: Path, verbose: Boolean): Unit = {
    val files = Files.list(dir).iterator.asScala.filter(_.toString.endsWith(".scala")).toList.sorted
    var crashes, extras, total = 0
    for (f <- files) {
      val base = f.getFileName.toString.stripSuffix(".scala")
      val flagsFile = dir.resolve(base + ".flags")
      val extraArgs =
        if (Files.exists(flagsFile)) new String(Files.readAllBytes(flagsFile)).split("\\s+").filter(_.nonEmpty).toList else Nil
      val code = new String(Files.readAllBytes(f))
      total += 1
      val c = try compare(code, extraArgs.filterNot(a => a.startsWith("-Xplugin") || a.startsWith("-Xfatal") || a == "-Werror")) catch { case t: Throwable => println(s"[$base] HARNESS FAILURE $t"); null }
      if (c != null) {
        if (c.withFlag.crash.isDefined && c.baseline.crash.isEmpty) {
          crashes += 1
          println(s"[$base] CRASH with flag: ${c.withFlag.crash.get}")
        }
        if (c.extra.nonEmpty) {
          extras += 1
          if (verbose) println(s"[$base] extra:\n  ${c.extra.mkString("\n  ")}")
          else println(s"[$base] ${c.extra.size} extra")
        }
      }
    }
    println(s"scanned $total files; $crashes new crashes; $extras files with extra diagnostics")
  }
}

object RefchecksAfterErrorsCorpusScan {
  def main(args: Array[String]): Unit = {
    val verbose = args.contains("-v")
    args.filterNot(_ == "-v").foreach(d => RefchecksAfterErrorsHarness.scan(Paths.get(d), verbose))
  }
}
