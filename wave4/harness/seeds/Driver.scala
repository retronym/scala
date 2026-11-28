package w4s

import java.io._
import java.net.URLClassLoader
import java.nio.file.{Files, Path}
import java.util.concurrent.{Callable, Executors, TimeUnit, TimeoutException}
import scala.reflect.internal.util.{AbstractFileClassLoader, BatchSourceFile}
import scala.reflect.io.VirtualDirectory
import scala.tools.nsc.{Global, Settings}
import scala.tools.nsc.reporters.StoreReporter

/** Compiles and runs existing run tests in several configurations and writes one outcome file per (seed, configuration). */
object Driver {
  val jars: List[String] = sys.props("w4s.jars").split(File.pathSeparator).toList
  val work = new File(sys.props("w4s.work"))
  val pool = Executors.newCachedThreadPool { (r: Runnable) => val t = new Thread(null, r, "w", 16L << 20); t.setDaemon(true); t }
  def timed[A](secs: Int)(f: => A): Option[A] = {
    val fut = pool.submit(new Callable[A] { def call(): A = f })
    try Some(fut.get(secs, TimeUnit.SECONDS)) catch { case _: TimeoutException => fut.cancel(true); None }
  }
  def newGlobal(flags: List[String], cp: List[String], out: Either[VirtualDirectory, Path]): (Global, StoreReporter) = {
    val s = new Settings(_ => ())
    val (_, rest) = s.processArguments("-nowarn" :: flags, processAll = true)
    s.usejavacp.value = false
    s.classpath.value = (jars ::: cp).mkString(File.pathSeparator)
    out match { case Left(vd) => s.outputDirs.setSingleOutput(vd); case Right(p) => s.outputDirs.setSingleOutput(p.toString) }
    val rep = new StoreReporter(s)
    (new Global(s, rep), rep)
  }
  def compile(srcs: List[(String, String)], flags: List[String], cp: List[String], out: Either[VirtualDirectory, Path]): Option[String] = {
    val (g, rep) = newGlobal(flags, cp, out)
    try {
      new g.Run().compileSources(srcs.map { case (n, c) => new BatchSourceFile(n, c) })
      val errs = rep.infos.filter(_.severity == rep.ERROR).toList
      if (errs.isEmpty) None else Some(errs.map(i => i.pos.line + ": " + i.msg).mkString("\n"))
    } catch { case t: Throwable => Some("CRASH " + t.getClass.getName + ": " + String.valueOf(t.getMessage).linesIterator.take(3).mkString(" | ")) }
    finally g.close()
  }

  /** Top-level chunks of a source: (names defined or "" for imports, text including leading comments and annotations). Header is the package clause text. */
  def split(src: String): Option[(String, List[(String, String)])] = {
    val (g, _) = newGlobal(Nil, Nil, Left(new VirtualDirectory("o", None)))
    try {
      import g._
      new Run()
      val unit = new CompilationUnit(new BatchSourceFile("p.scala", src))
      val tree = newUnitParser(unit).parse()
      var stats = tree match { case PackageDef(_, ss) => ss; case _ => return None }
      var header = ""; var prevEnd = 0
      stats match {
        case List(pd @ PackageDef(pid, inner)) =>
          val e = pid.pos.end
          if (src.substring(e).trim.startsWith("{")) return None
          header = "package " + src.substring(pid.pos.start, e) + "\n"; prevEnd = e; stats = inner
        case _ if stats.exists(_.isInstanceOf[PackageDef]) => return None
        case _ =>
      }
      val chunks = stats.map { st =>
        val name = st match {
          case c: ClassDef => c.name.toString + (if (c.mods.isTrait) "" else "")
          case m: ModuleDef => if (m.name.toString == "package") return None else m.name.toString
          case _: Import => ""
          case _ => return None
        }
        val end = st.pos.end
        val text = src.substring(prevEnd, end); prevEnd = end
        (name, text)
      }
      Some((header, chunks))
    } catch { case t: Throwable => if (sys.props.contains("w4s.debug")) t.printStackTrace(); None } finally g.close()
  }
  /** Units (each preceded by earlier imports), companions merged. */
  def units(chunks: List[(String, String)]): List[String] = {
    var imports = List.empty[String]
    val order = scala.collection.mutable.LinkedHashMap[String, (List[String], StringBuilder)]()
    for ((n, t) <- chunks) {
      if (n.isEmpty) imports ::= t
      else { val e = order.getOrElseUpdate(n, (imports.reverse, new StringBuilder)); e._2.append(t).append("\n") }
    }
    order.values.map { case (imps, sb) => imps.mkString("\n") + "\n" + sb }.toList
  }

  def runMain(loader: ClassLoader): (String, String) = {
    val buf = new ByteArrayOutputStream
    val ps = new PrintStream(buf, true, "UTF-8")
    val so = System.out; val si = System.in
    System.setOut(ps); System.setIn(new ByteArrayInputStream(Array.emptyByteArray))
    var kind = "ok"
    try {
      scala.Console.withOut(ps) {
        try {
          val m = try loader.loadClass("Test").getMethod("main", classOf[Array[String]]).invoke(null, Array.empty[String]) catch { case _: NoSuchMethodException =>
            val c = loader.loadClass("Test$"); c.getMethod("main", classOf[Array[String]]).invoke(c.getField("MODULE$").get(null), Array.empty[String]) }
        } catch {
          case e: java.lang.reflect.InvocationTargetException => kind = "throw:" + e.getCause.getClass.getName
          case e: Throwable => kind = "throw:" + e.getClass.getName
        }
      }
    } finally { System.setOut(so); System.setIn(si); ps.flush() }
    (kind, buf.toString("UTF-8"))
  }

  var sepN = 0
  /** One configuration. Result kind: ok, throw:<class>, compile-error, crash, timeout-compile, timeout-run, skip. */
  def eval(src: String, flags: List[String], separate: Option[Int]): (String, String) = {
    val parent = classOf[Option[_]].getClassLoader
    def go(): (String, String) = {
      separate match {
        case None =>
          val vd = new VirtualDirectory("out", None)
          compile(List("p.scala" -> src), flags, Nil, Left(vd)) match {
            case Some(e) => (if (e.startsWith("CRASH")) "crash" else "compile-error", e)
            case None => runIt(new AbstractFileClassLoader(vd, parent))
          }
        case Some(rev) =>
          val (header, chunks) = split(src).getOrElse(return ("skip", "cannot split"))
          var us = units(chunks)
          if (us.size < 2) return ("skip", "single unit")
          if (rev == 1) us = us.reverse
          sepN += 1
          val dir = Files.createDirectories(work.toPath.resolve("sep" + sepN))
          try {
            // rev == 2: dependency order, retrying units that failed until no progress (companions stay together; sealed hierarchies cannot be split and fail)
            var pending = us.zipWithIndex; var lastErr = ""
            var progress = true
            while (pending.nonEmpty && progress) {
              progress = false
              for ((u, i) <- pending) compile(List(s"u$i.scala" -> (header + u)), flags, List(dir.toString), Right(dir)) match {
                case Some(e) if e.startsWith("CRASH") => return ("crash", s"unit $i: " + e)
                case Some(e) => lastErr = s"unit $i: " + e; if (rev != 2) return ("compile-error", lastErr)
                case None => pending = pending.filterNot(_._2 == i); progress = true
              }
            }
            if (pending.nonEmpty) return ("compile-error", lastErr)
            val l = new URLClassLoader(Array(dir.toUri.toURL), parent)
            try runIt(l) finally l.close()
          } finally {
            val fs = Files.walk(dir); try fs.sorted(java.util.Comparator.reverseOrder[Path]()).forEach(p => Files.delete(p)) finally fs.close()
          }
      }
    }
    timed(60)(go()).getOrElse(("timeout-compile", ""))
  }
  def runIt(l: ClassLoader): (String, String) = timed(20)(runMain(l)).getOrElse(("timeout-run", ""))

  val specRx = "@(scala\\.)?specialized(\\s*\\([^)]*\\))?"
  def main(args: Array[String]): Unit = {
    val list = args(0); val outDir = new File(args(1)); val cfgs = args(2).split(",").toList
    val part = args(3).toInt; val parts = args(4).toInt; val secs = args(5).toInt
    outDir.mkdirs(); work.mkdirs()
    val deadline = System.nanoTime + secs * 1000000000L
    val seeds = scala.io.Source.fromFile(list).getLines().toList.zipWithIndex.filter(_._2 % parts == part).map(_._1.split("\t", -1))
    var timeouts = 0
    for (a <- seeds if System.nanoTime < deadline && timeouts < 3) {
      val name = a(0).replace('/', '_'); val opts = if (a(2).isEmpty) Nil else a(2).split(" ").toList
      val src = new String(Files.readAllBytes(new File(a(1)).toPath), "UTF-8")
      for (c <- cfgs) {
        val f = new File(outDir, s"$name.$c.txt")
        if (!f.exists) {
          val t0 = System.nanoTime
          val r: (String, String) = c match {
            case "base" => eval(src, opts, None)
            case "optlocal" => eval(src, "-opt:local" :: opts, None)
            case "delam" => eval(src, "-Ydelambdafy:inline" :: opts, None)
            case "xsource3" => eval(src, "-Xsource:3" :: opts, None)
            case "nospec" => val s2 = src.replaceAll(specRx, ""); if (s2 == src) ("skip", "no specialized") else eval(s2, opts, None)
            case "optinline" => eval(src, "-opt:inline:**" :: "-opt:local" :: opts, None)
            case "ycheck" => eval(src, "-Ycheck:all" :: opts, None)
            case "optdelam" => eval(src, "-opt:local" :: "-Ydelambdafy:inline" :: opts, None)
            case "sepfwd" => eval(src, opts, Some(0))
            case "seprev" => eval(src, opts, Some(1))
            case "sepdep" => eval(src, opts, Some(2))
          }
          if (r._1.startsWith("timeout")) timeouts += 1
          Files.write(f.toPath, (r._1 + "\n" + r._2).getBytes("UTF-8"))
          println(f"${a(0)} $c ${r._1} ${(System.nanoTime - t0) / 1e9}%.1fs")
        }
      }
    }
    println("DONE part " + part + (if (timeouts >= 3) " (stopped after timeouts)" else ""))
    System.exit(if (timeouts >= 3) 3 else 0)
  }
}
