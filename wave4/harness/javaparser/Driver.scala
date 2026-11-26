// Differential driver for scalac's Java source parser. See README.md.
import java.io.{ByteArrayOutputStream, File, PrintStream, StringWriter}
import java.net.URLClassLoader
import java.nio.file.{Files, Path, Paths}
import scala.collection.mutable.ListBuffer
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._
import scala.reflect.internal.Flags
import scala.reflect.internal.util.BatchSourceFile
import scala.tools.nsc.{Global, Settings}
import scala.tools.nsc.reporters.StoreReporter

object Driver {
  val libJar = classOf[Option[_]].getProtectionDomain.getCodeSource.getLocation.getPath
  val reflectJar = classOf[scala.reflect.api.Universe].getProtectionDomain.getCodeSource.getLocation.getPath

  def files(dir: Path, ext: String): List[String] =
    if (!Files.exists(dir)) Nil
    else Files.walk(dir).iterator.asScala.map(_.toString).filter(_.endsWith(ext)).toList.sorted

  def javac(srcs: List[String], out: Path, cp: String = ""): Option[String] = {
    Files.createDirectories(out)
    val jc = javax.tools.ToolProvider.getSystemJavaCompiler
    val sw = new StringWriter
    val fm = jc.getStandardFileManager(null, null, null)
    val ok = jc.getTask(sw, fm, null, (List("-parameters", "-nowarn", "-Xlint:none", "-proc:none", "-d", out.toString) ++ (if (cp.isEmpty) Nil else List("-cp", cp))).asJava, null,
      fm.getJavaFileObjectsFromStrings(srcs.asJava)).call()
    fm.close()
    if (ok) None else Some(sw.toString)
  }

  def mkGlobal(cp: String, out: Option[Path], extra: List[String] = Nil): (Global, StoreReporter) = {
    val s = new Settings(msg => sys.error(msg))
    s.processArguments(List("-usejavacp:false", "-cp", cp, "-deprecation", "-unchecked") ++ extra, processAll = true)
    out.foreach { o => Files.createDirectories(o); s.outputDirs.setSingleOutput(o.toString) }
    val r = new StoreReporter(s)
    (new Global(s, r), r)
  }

  val mask: Long = Flags.PROTECTED | Flags.FINAL | Flags.ABSTRACT | Flags.DEFERRED | Flags.STATIC | Flags.INTERFACE |
    Flags.JAVA_ENUM | Flags.JAVA_ANNOTATION | Flags.JAVA_DEFAULTMETHOD | Flags.SEALED | Flags.MUTABLE | Flags.VARARGS | Flags.TRAIT | Flags.JAVA

  /** One line per non-private member of every class in package `pkg`. */
  def noiseAnnot(a: String) = a == "transient" || a == "scala.annotation.strictfp" || a.startsWith("SuppressWarnings") || a.startsWith("SafeVarargs") || a.startsWith("Override")
  def dump(g: Global, pkg: String): List[String] = { val ps = List(pkg, pkg + ".a", pkg + ".b"); ps.foreach(dump0(g, _)); ps.flatMap(dump0(g, _)) } // the first pass completes every symbol
  def dump0(g: Global, pkg: String): List[String] = {
    import g._
    val buf = ListBuffer.empty[String]
    def const(t: Type): String = t.resultType match {
      case ConstantType(c) => " const=" + c.tag + ":" + (c.value match {
        case f: Float => "f" + java.lang.Float.floatToRawIntBits(f)
        case d: Double => "d" + java.lang.Double.doubleToRawLongBits(d)
        case ch: Char => "c" + ch.toInt
        case s: String => "s" + s.map(_.toInt).mkString(",")
        case x => String.valueOf(x) })
      case _ => ""
    }
    def common(s: Symbol): String = {
      val pw = if (s.hasAccessBoundary) " within=" + s.privateWithin.name else ""
      val ann = exitingTyper(s.annotations.map { a => a.completeInfo(); val t = a.toString; if (t.startsWith("throws[")) t.take(t.indexOf(']') + 1) else t }).filterNot(noiseAnnot).sorted.mkString(" @[", ";", "]")
      // a final Java field is a non-mutable val on both sides, but only the class file parser also sets FINAL
      val m = if (s.isMethod || s.isClass) mask else mask & ~Flags.FINAL
      "flags=<" + s.flagString(m) + ">" + pw + ann
    }
    def member(prefix: String, s: Symbol): Unit = {
      val tp = exitingTyper(s.info)
      val gen = exitingTyper(tp.toString)
      val er = exitingErasure(s.info.toString)
      val kind = if (s.isMethod) "def" else "val"
      buf += s"$prefix$kind ${s.name}\u0001${common(s)} : $gen erased $er${const(tp)}"
    }
    def klass(s: Symbol): Unit = {
      exitingTyper(s.initialize)
      val nm = exitingTyper(s.fullName)
      val tps = exitingTyper(s.typeParams.map(tp => tp.name.toString + tp.info).mkString("[", ",", "]"))
      val ps = exitingTyper(s.info.parents.mkString(" with "))
      val kids = exitingTyper(s.knownDirectSubclasses.toList.map(_.fullName).sorted.mkString(","))
      buf += s"class $nm\u0001${common(s)} $tps extends $ps children=$kids"
      def decls(c: Symbol, pre: String): List[Symbol] = exitingTyper(c.info.decls.toList)
      val inst = decls(s, "")
      val stat = if (s.companionModule == NoSymbol) Nil else decls(exitingTyper(s.companionModule.moduleClass), "")
      for ((ms, pre) <- List(inst -> s"$nm#", stat -> s"$nm."); m <- ms.sortBy(m => exitingTyper(m.name.toString + m.info)))
        if (m.isConstructor && (pre.endsWith(".") || s.isInterface)) ()
        else if (m.isPrivate || m.name == nme.CLASS_CONSTRUCTOR || m.isSynthetic && !m.isClass && !m.isConstructor || m.name.toString.contains("$")) ()
        else if (m.isClass) klass(m)
        else if (m.isModule) ()
        else member(pre, m)
    }
    val p = rootMirror.getPackageIfDefined(TermName(pkg))
    if (p == NoSymbol) { if (!pkg.contains('.')) buf += "no package" }
    else exitingTyper(p.info.decls.toList).filter(c => c.isClass && !c.name.toString.contains("$") && c.name.toString != "package-info").sortBy(_.name.toString).foreach(klass)
    buf.toList
  }

  def run(dirs: List[Path], parent: ClassLoader, cls: String): String = {
    val bos = new ByteArrayOutputStream
    val ps = new PrintStream(bos, true, "UTF-8")
    val cl = new URLClassLoader(dirs.map(_.toUri.toURL).toArray, parent)
    try Console.withOut(ps) { cl.loadClass(cls).getMethod("main", classOf[Array[String]]).invoke(null, Array.empty[String]) }
    catch { case t: Throwable => ps.println("THROWN " + Option(t.getCause).getOrElse(t)) }
    finally cl.close()
    bos.toString("UTF-8")
  }

  def msgs(r: StoreReporter): List[String] =
    r.infos.toList.map(i => i.severity.toString + ": " + i.msg.linesIterator.take(2).mkString(" | ")).sorted

  def oneCase(dir: Path, work: Path): List[String] = {
    val pkg = dir.getFileName.toString
    val javas = files(dir.resolve("src"), ".java")
    val probe = dir.resolve("Probe.scala").toString
    val cls = work.resolve("cls")
    // Scala definitions the Java sources refer to: compiled first for javac and the separate compilations, given as source to the joint ones
    val defs = dir.resolve("Defs.scala").toString
    val hasDefs = Files.exists(Paths.get(defs))
    val sdefs = work.resolve("sdefs")
    val defsSrc = if (hasDefs) List(defs) else Nil
    val defsCp = if (hasDefs) File.pathSeparator + sdefs else ""
    if (hasDefs) {
      val (g, r) = mkGlobal(libJar, Some(sdefs))
      new g.Run().compile(List(defs))
      if (r.hasErrors) return List("DEFS-REJECT " + msgs(r).take(2).mkString(" | "))
    }
    javac(javas, cls, if (hasDefs) libJar + defsCp else "") match {
      case Some(err) => return List("JAVAC-REJECT " + err.linesIterator.take(3).mkString(" | "))
      case None =>
    }
    val out = ListBuffer.empty[String]
    // oracle 2: symbols from source against symbols from the class files
    val srcDump = try {
      val (g, r) = mkGlobal(libJar, Some(work.resolve("o2")))
      new g.Run().compile(javas ++ defsSrc)
      if (r.hasErrors) msgs(r).filter(_.startsWith("ERROR")).map("SRC-ERROR " + _) else dump(g, pkg)
    } catch { case t: Throwable => List("SRC-CRASH " + t.toString.linesIterator.take(2).mkString(" | ") + " at " + t.getStackTrace.take(12).mkString(" < ")) }
    val clsDump = try {
      val (g, _) = mkGlobal(libJar + File.pathSeparator + cls + defsCp, Some(work.resolve("o2c")))
      new g.Run().compileSources(List(new BatchSourceFile("empty.scala", "class Empty$Unit")))
      dump(g, pkg)
    } catch { case t: Throwable => List("CLS-CRASH " + t.toString.linesIterator.take(2).mkString(" | ") + " at " + t.getStackTrace.take(12).mkString(" < ")) }
    if (srcDump.exists(_.startsWith("SRC-")) || clsDump.exists(_.startsWith("CLS-"))) (srcDump ++ clsDump).filter(l => l.startsWith("SRC-") || l.startsWith("CLS-")).foreach(out += _)
    else {
      def key(l: String) = l.takeWhile(_ != '\u0001')
      val (s, c) = (srcDump.groupBy(key), clsDump.groupBy(key))
      for (k <- (s.keySet ++ c.keySet).toList.sorted) {
        val (ls, lc) = (s.getOrElse(k, Nil).map(_.drop(k.length + 1)), c.getOrElse(k, Nil).map(_.drop(k.length + 1)))
        val (os, oc) = (ls.filterNot(lc.toSet), lc.filterNot(ls.toSet))
        if (os.nonEmpty || oc.nonEmpty) out += s"SYM $k :: src [${os.mkString(" ## ")}] cls [${oc.mkString(" ## ")}]"
      }
    }
    // oracle 1: probe compiled jointly (A) and against the class files (B)
    if (Files.exists(Paths.get(probe))) {
      def compile(tag: String, srcs: List[String], cp: String): (Boolean, List[String], String) = try {
        val o = work.resolve(tag)
        val (g, r) = mkGlobal(cp, Some(o))
        new g.Run().compile(srcs)
        val ok = !r.hasErrors
        (ok, msgs(r), if (ok) run(List(o, cls) ++ (if (hasDefs) List(sdefs) else Nil), getClass.getClassLoader, pkg + ".Probe") else "")
      } catch { case t: Throwable => (false, List("CRASH " + t.toString.linesIterator.take(2).mkString(" | ") + " at " + t.getStackTrace.take(4).mkString(" < ")), "") }
      val a = compile("a", probe :: javas ++ defsSrc, libJar + File.pathSeparator + reflectJar)
      val b = compile("b", List(probe), libJar + File.pathSeparator + reflectJar + File.pathSeparator + cls + defsCp)
      if (a._1 != b._1) out += s"PROBE compile differs: joint=${a._1} separate=${b._1}"
      if (a._1 && b._1 && a._2 != b._2) {
        a._2.filterNot(b._2.toSet).foreach(m => out += "PROBE joint only: " + m)
        b._2.filterNot(a._2.toSet).foreach(m => out += "PROBE separate only: " + m)
      }
      if (a._1 && b._1 && a._3 != b._3) {
        val (la, lb) = (a._3.linesIterator.toList, b._3.linesIterator.toList)
        la.zipAll(lb, "<none>", "<none>").filter(p => p._1 != p._2).take(8).foreach(p => out += s"PROBE output: joint [${p._1}] separate [${p._2}]")
      }
      if (!a._1 && !b._1) out += "PROBE-BOTH-REJECT " + b._2.filter(_.startsWith("ERROR")).take(2).mkString(" | ")
      if (a._3.contains("THROWN") && a._3 == b._3) out += "PROBE-BOTH-THROW " + a._3.linesIterator.filter(_.contains("THROWN")).mkString
    }
    out.toList
  }

  /** Known differences, excluded narrowly so they do not mask what is behind them; each exclusion is counted. */
  val known: List[(String, scala.util.matching.Regex)] = List(
    "record-members-explicit-in-classfile" -> """SYM \S+#def (toString|hashCode|equals) :: src \[\] cls \[flags=<final <java>>.*""".r,
    "bounded-T-array-prints-with-Object" -> """SYM .+? :: src \[.*Array\[T with Object\].*\] cls \[.*""".r,
    "GJ1-sealed-without-permits" -> """(SYM class \S+ :: src \[.*children=\] cls \[.*children=\S+.*)|(PROBE joint only: WARNING: unreachable code)|(PROBE separate only: WARNING: match may not be exhaustive.*)""".r,
    "WJ1-primitive-or-array-class-literal-in-annotation" -> """SYM .+? :: src \[flags=<[^]]*> @\[\] .*\] cls \[.*classOf\[(scala\.(Int|Boolean|Unit|Long|Char|Byte|Short|Float|Double)|\[).*""".r,
    "GJ2-varargs-record-accessor" -> """SYM .+?#def \w+ :: src \[.*: \(\): \w+\* erased.*""".r)
  val knownCount = scala.collection.mutable.Map.empty[String, Int].withDefaultValue(0)
  def dropKnown(ls: List[String]): List[String] = ls.filter { l =>
    known.find(_._2.matches(l)) match { case Some((k, _)) => knownCount(k) += 1; false; case None => true }
  }

  def main(args: Array[String]): Unit = {
    val root = Paths.get(args(0)); val work = Paths.get(args(1))
    val cases = Files.list(root).iterator.asScala.filter(Files.isDirectory(_)).toList.sortBy(_.getFileName.toString)
    var rejects, clean, failing, hung = 0
    val t0 = System.nanoTime
    import scala.concurrent.ExecutionContext.Implicits.global
    for (c <- cases) {
      val w = work.resolve(c.getFileName.toString)
      val res = dropKnown(try Await.result(Future(oneCase(c, w)), 60.seconds) catch { case t: Throwable => hung += 1; List("TIMEOUT-OR-CRASH " + t) })
      if (res.exists(_.startsWith("JAVAC-REJECT"))) rejects += 1
      else if (res.isEmpty) clean += 1 else failing += 1
      res.foreach(l => println(c.getFileName.toString + " " + l))
    }
    println("==== known excluded: " + knownCount.toList.sorted.mkString(", "))
    println(f"==== cases=${cases.size} javacRejects=$rejects clean=$clean failing=$failing hung=$hung seconds=${(System.nanoTime - t0) / 1e9}%.1f")
  }
}
