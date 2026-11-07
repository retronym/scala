import scala.tools.nsc._, scala.tools.nsc.reporters.StoreReporter, java.nio.file._, scala.jdk.CollectionConverters._, java.security.MessageDigest
/** One edit, recompiled round by round as Zinc would: a fresh Global per round, one kept Global, one kept Global with the output directory invalidated. */
object Ripple {
  val base = Map(
    "A.scala" -> "package p\nobject A { def f = 1; class In { def v = f }; def twice(g: Int => Int) = g(g(f)) }\ntrait T { val tv = A.f; def m = tv + 1; private def hidden = 3; def useHidden = hidden }\ncase class CC(a: Int, b: String = \"x\")\n",
    "B.scala" -> "package p\nobject B { def g = A.f; def h = A.twice(_ + 1); val cc = CC(1) }\nclass K extends T { override def m = super.m + B.g.hashCode }\nclass Sp[@specialized(Int) X](val x: X) { def get = x }\n",
    "C.scala" -> "package p\nobject C { val x = B.g; def k = new K().m; def s = new Sp(1).get; def cp = B.cc.copy(b = \"y\") }\n",
    "D.scala" -> "package p\nobject D { val y: Any = C.x; def z = C.k + C.s; def in = new A.In().v }\n")
  val editA = base("A.scala").replace("def f = 1", "def f = \"s\"").replace("val tv = A.f; def m = tv + 1", "val tv = A.f; def m = tv.length + 1").replace("def v = f", "def v = f.length").replace("g(g(f))", "g(g(f.length))")
  def write(src: Path, m: Map[String,String]) = { Files.createDirectories(src); m.foreach { case (k, v) => Files.writeString(src.resolve(k), v) } }
  def digest(d: Path): Map[String,String] = { val s = Files.walk(d); try s.iterator.asScala.filter(Files.isRegularFile(_)).map(p => d.relativize(p).toString -> MessageDigest.getInstance("SHA-1").digest(Files.readAllBytes(p)).map("%02x".format(_)).mkString).toMap finally s.close() }
  def clean(d: Path) = if (Files.exists(d)) { val s = Files.walk(d); try s.iterator.asScala.toList.reverse.foreach(Files.delete) finally s.close() }
  def mk(cp: String, out: Path) = { val st = new Settings(println); val c = new CompilerCommand(List("-nowarn","-cp", cp + ":" + out, "-d", out.toString), st); new Global(c.settings, new StoreReporter(c.settings)) }
  def go(g: Global, src: Path, files: List[String]) = { g.reporter.reset(); val r = new g.Run; r.compile(files.map(f => src.resolve(f).toString)); val rr = g.reporter.asInstanceOf[StoreReporter]; val errs = rr.infos.filter(_.severity == rr.ERROR).map(i => i.msg.take(160)).toList; rr.reset(); errs }
  def prune(out: Path, prefixes: List[String]) = { val s = Files.walk(out); try s.iterator.asScala.filter(p => Files.isRegularFile(p) && prefixes.exists { q => val n = p.getFileName.toString; n == q + ".class" || n.startsWith(q + "$") }).toList.foreach(Files.delete) finally s.close() }
  val products = Map("A.scala" -> List("A","T","CC"), "B.scala" -> List("B","K","Sp"), "C.scala" -> List("C"), "D.scala" -> List("D"))
  def main(a: Array[String]): Unit = {
    val cp = a(0); val root = Paths.get(a(1)); clean(root)
    val src = root.resolve("src"); write(src, base)
    val all = base.keys.toList.sorted
    val cleanOut = root.resolve("clean"); Files.createDirectories(cleanOut)
    write(src, base.updated("A.scala", editA)); println("clean errors " + go(mk(cp, cleanOut), src, all)); val want = digest(cleanOut); write(src, base)
    val rounds = List(List("A.scala"), List("B.scala"), List("C.scala", "D.scala"))
    for (mode <- List("fresh", "kept", "kept+invalidate")) {
      val out = root.resolve(mode); Files.createDirectories(out)
      var kept: Global = null
      def g() = if (mode == "fresh") mk(cp, out) else { if (kept == null) kept = mk(cp, out); kept }
      println(s"$mode round0 errors " + go(g(), src, all))
      write(src, base.updated("A.scala", editA))
      for (r <- rounds) {
        prune(out, r.flatMap(products))
        if (mode == "kept+invalidate") kept.invalidateClassPathEntries(out.toString)
        println(s"$mode round $r errors " + go(g(), src, r))
      }
      val got = digest(out)
      val diff = (want.keySet ++ got.keySet).filter(k => want.get(k) != got.get(k)).toList.sorted
      println(s"$mode: ${got.size} files, differ from clean: ${diff.size} ${diff.mkString(" ")}")
      write(src, base)
    }
  }
}
