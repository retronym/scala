package w4e

import java.io.File
import java.net.URLClassLoader
import java.nio.file.{Files, Path}
import java.util.concurrent.{Callable, Executors, TimeUnit, TimeoutException}
import scala.collection.mutable
import scala.reflect.internal.util.{AbstractFileClassLoader, BatchSourceFile}
import scala.reflect.io.VirtualDirectory
import scala.tools.nsc.{Global, Settings}
import scala.tools.nsc.reporters.StoreReporter

case class Outcome(kind: String, text: String)
case class Variant(name: String, rewrite: String => String = identity, flags: List[String] = Nil, separate: Boolean = false)

object Driver {
  val libJar: String = sys.props("w4e.lib")
  val pool = Executors.newCachedThreadPool { (r: Runnable) => val t = new Thread(null, r, "w", 16L << 20); t.setDaemon(true); t }
  def timed[A](secs: Int)(f: => A): Option[A] = {
    val fut = pool.submit(new Callable[A] { def call(): A = f })
    try Some(fut.get(secs, TimeUnit.SECONDS)) catch { case _: TimeoutException => fut.cancel(true); None }
  }
  var compiles = 0
  def compileOnce(srcs: List[(String, String)], flags: List[String], cp: List[String], out: Either[VirtualDirectory, Path]): Option[String] = {
    compiles += 1
    val s = new Settings(_ => ())
    s.processArguments("-nowarn" :: flags, processAll = true)
    s.usejavacp.value = false
    s.classpath.value = (libJar :: cp).mkString(File.pathSeparator)
    out match { case Left(vd) => s.outputDirs.setSingleOutput(vd); case Right(p) => s.outputDirs.setSingleOutput(p.toString) }
    val rep = new StoreReporter(s)
    val g = new Global(s, rep)
    try {
      new g.Run().compileSources(srcs.map { case (n, c) => new BatchSourceFile(n, c) })
      val errs = rep.infos.filter(_.severity == rep.ERROR).toList
      if (errs.isEmpty) None else Some(errs.map(i => i.pos.line + ": " + i.msg).mkString("\n"))
    } catch { case t: Throwable => Some("CRASH " + t.getClass.getName + ": " + String.valueOf(t.getMessage).linesIterator.take(3).mkString(" | ")) }
    finally g.close()
  }
  val sepRoot: Path = new File(sys.props("w4e.work")).toPath
  var sepN = 0
  def run(loader: ClassLoader): Outcome =
    timed(20) {
      try Outcome("ok", loader.loadClass("Main").getMethod("run").invoke(null).asInstanceOf[String])
      catch { case e: java.lang.reflect.InvocationTargetException => Outcome("run-throw", e.getCause.getClass.getName)
              case e: Throwable => Outcome("run-throw", e.getClass.getName) }
    }.getOrElse(Outcome("timeout", "run"))

  val times = mutable.LinkedHashMap[String, Long]()
  def eval(units: List[String], v: Variant): Outcome = {
    val t0 = System.nanoTime
    try eval0(units, v) finally times(v.name) = times.getOrElse(v.name, 0L) + (System.nanoTime - t0) / 1000000
  }
  def eval0(units: List[String], v: Variant): Outcome = {
    val us = units.map(v.rewrite)
    val parent = classOf[Option[_]].getClassLoader
    val res = timed(60) {
      if (!v.separate) {
        val vd = new VirtualDirectory("out", None)
        compileOnce(List("p.scala" -> us.mkString("\n")), v.flags, Nil, Left(vd)).toLeft(new AbstractFileClassLoader(vd, parent): ClassLoader)
      } else {
        sepN += 1
        val dir = Files.createDirectories(sepRoot.resolve("sep" + sepN))
        val err = us.zipWithIndex.iterator.map { case (u, i) => compileOnce(List(s"u$i.scala" -> u), v.flags, List(dir.toString), Right(dir)) }.collectFirst { case Some(e) => e }
        err.toLeft(new URLClassLoader(Array(dir.toUri.toURL), parent): ClassLoader)
      }
    }
    val out = res match {
      case None => Outcome("timeout", "compile")
      case Some(Left(e)) => Outcome(if (e.startsWith("CRASH")) "crash" else "compile-error", e)
      case Some(Right(l)) => try run(l) finally l match { case u: URLClassLoader => u.close(); case _ => }
    }
    if (v.separate) { // remove the class files this call wrote
      val dir = sepRoot.resolve("sep" + sepN)
      val fs = Files.list(dir); try fs.forEach(f => Files.delete(f)) finally fs.close(); Files.delete(dir)
    }
    out
  }

  val specRx = "@specialized(\\([^)]*\\))? "
  def variants(sample: Boolean, inl: Boolean): List[Variant] = List(
    Variant("nospec", _.replaceAll(specRx, "")),
    Variant("novc", _.replace(" extends AnyVal", " extends AnyRef")),
    Variant("separate", separate = true),
    Variant("delambdafy-inline", flags = List("-Ydelambdafy:inline"))) :::
    (if (inl) List(Variant("inline", flags = List("-opt:inline:**", "-opt:local"))) else Nil) :::
    (if (sample) List(Variant("ycheck", flags = List("-Ycheck:all"))) else Nil)

  def normErr(e: String): String = e.linesIterator.next().replaceAll("^\\d+: ", "").replaceAll("[A-Za-z]+\\d+", "N").take(90)
  /** Signature of a difference between the reference outcome and a variant's, or None when they agree. */
  def signature(base: Outcome, v: Variant, o: Outcome): Option[String] =
    if (base == o || (base.kind == "compile-error" && o.kind == "compile-error")) None
    else if (o.kind != "ok" || base.kind != "ok") Some(s"${v.name}|${base.kind}/${o.kind}|" + (if (o.kind == "ok") normErr(base.text) else normErr(o.text)))
    else {
      val (a, b) = (base.text.linesIterator.toList, o.text.linesIterator.toList)
      val d = a.zipAll(b, "", "").find { case (x, y) => x != y }.get
      def exc(s: String) = if (s.contains("!")) s.substring(s.indexOf('!') + 1) else "value"
      Some(s"${v.name}|diff|${exc(d._1)}/${exc(d._2)}")
    }

  def sigOf(units: List[String], v: Variant): Option[String] = {
    val base = eval(units, Variant("base"))
    val o = eval(units, v)
    if (base.kind == "compile-error" && o.kind != "ok") None else signature(base, v, o)
  }
  /** Delete units, then lines, while the reference still compiles and the signature is unchanged. */
  def shrink(units0: List[List[String]], v: Variant, sig: String, budget: Int): List[List[String]] = {
    var units = units0; var left = budget
    def ok(c: List[List[String]]) = { left -= 1; if (System.nanoTime > deadline) left = 0; left >= 0 && sigOf(c.map(_.mkString("\n")), v).contains(sig) }
    var progress = true
    while (progress && left > 0) {
      progress = false
      var i = units.length - 2 // never the Main unit
      while (i >= 0 && left > 0) { val c = units.patch(i, Nil, 1); if (ok(c)) { units = c; progress = true }; i -= 1 }
      var u = units.length - 1
      while (u >= 0 && left > 0) {
        var j = units(u).length - 1
        while (j >= 1 && left > 0) {
          if (j < units(u).length) {
            val line = units(u)(j)
            if (line.startsWith("    ") || line.startsWith("  ") && !line.startsWith("  def run") && line.trim != "}") {
              val c = units.updated(u, units(u).patch(j, Nil, 1))
              if (ok(c)) { units = c; progress = true }
            }
          }
          j -= 1
        }
        u -= 1
      }
    }
    units
  }

  var deadline = Long.MaxValue
  def main(args: Array[String]): Unit = {
    val from = args(0).toLong; val secs = args(1).toInt; val outDir = new File(args(2)); outDir.mkdirs()
    val opts = args.drop(3).toSet
    deadline = System.nanoTime + secs * 1000000000L
    var timeouts = 0
    val sigs = mutable.LinkedHashMap[String, (Int, Long)]()
    var n = 0; var compiled = 0; var abandoned = 0
    val errs = mutable.LinkedHashMap[String, (Int, Long)]()
    var seed = from
    val warm = System.nanoTime
    while (System.nanoTime < deadline && timeouts < 3) {
      val (us, k) = Gen.make(seed); abandoned += k
      val units = us.map(_.mkString("\n"))
      n += 1
      val base = eval(units, Variant("base"))
      if (base.kind == "compile-error" || base.kind == "crash" || base.kind == "timeout") {
        val key = base.kind + " " + normErr(base.text); errs(key) = (errs.get(key).fold(1)(_._1 + 1), errs.get(key).fold(seed)(_._2))
        if (base.kind != "compile-error") Files.write(new File(outDir, s"basefail-$seed.scala").toPath, (units.mkString("\n//#unit\n") + "\n// " + base.text).getBytes)
      }
      if (base.kind == "run-throw") println(s"BASE-RUN-THROW seed=$seed ${base.text}")
      // a reference that does not compile is the generator's fault only if the rewritten programs do not compile either
      val rescued = base.kind == "compile-error" && { val vs = variants(false, false).take(2); vs.exists(v => eval(units, v).kind == "ok") }
      if (rescued) errs.remove(base.kind + " " + normErr(base.text))
      if (base.kind != "compile-error" || rescued) {
        compiled += 1
        if (opts("dump")) Files.write(new File(outDir, s"out-$seed.txt").toPath, (base.kind + "\n" + base.text).getBytes)
        for (v <- (if (opts("dump")) Nil else variants(sample = opts("ycheck") || seed % 10 == 0, inl = opts("inline") || seed % 8 == 0)); sig <- signature(base, v, eval(units, v))) {
          val first = !sigs.contains(sig)
          sigs(sig) = (sigs.get(sig).fold(1)(_._1 + 1), sigs.get(sig).fold(seed)(_._2))
          if (sig.contains("timeout")) timeouts += 1
          if (first) {
            println(s"NEW $sig seed=$seed")
            val small = if (opts("noshrink") || sig.contains("timeout")) us else shrink(us, v, sig, 120)
            val f = new File(outDir, s"sig-${sigs.size}-seed$seed-${v.name}.scala")
            Files.write(f.toPath, (s"// $sig\n" + small.map(_.mkString("\n")).mkString("\n//#unit\n") + "\n").getBytes)
          }
        }
      }
      seed += 1
    }
    val mins = (System.nanoTime - warm) / 6e10
    println(f"==== programs=$n compiled=$compiled (${100.0 * compiled / n}%.1f%%) generator-abandoned=$abandoned compiles=$compiles programs/min=${n / mins}%.1f seeds=$from..${seed - 1}")
    println("TIMES(ms) " + times.mkString(" "))
    errs.toList.sortBy(-_._2._1).foreach { case (k, (c, s)) => println(s"BASEFAIL $c first-seed=$s $k") }
    sigs.foreach { case (k, (c, s)) => println(s"SIG $c first-seed=$s $k") }
    System.exit(0)
  }
}
