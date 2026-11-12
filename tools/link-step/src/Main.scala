package linkstep

import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters._
import scala.collection.mutable

/** Regenerates the ancestor-derived members of every pickled class in a corpus directory and compares them with the
 *  class files there.
 *
 *  Usage: Main --corpus DIR --cp CLASSPATH [--label NAME] [--batch N] [--out DIR] [--only PREFIX] [-- scalac options]
 *
 *  `--cp` is the corpus's dependency class path (the corpus directory is put in front of it). Writes `<label>.tsv`
 *  (one line per diff), `<label>-classes.tsv` (one line per class) and `<label>-summary.txt` to `--out`.
 */
object Main {
  def main(args: Array[String]): Unit = {
    val (opts, scalacOpts) = args.toList.span(_ != "--")
    val kv = opts.grouped(2).collect { case List(k, v) => k -> v }.toMap
    val corpus = Paths.get(kv("--corpus"))
    val cp = corpus.toString + java.io.File.pathSeparator + kv.getOrElse("--cp", "")
    val label = kv.getOrElse("--label", corpus.getFileName.toString)
    val batch = kv.getOrElse("--batch", "400").toInt
    val outDir = Paths.get(kv.getOrElse("--out", "."))
    val only = kv.get("--only")
    val forceSpec = kv.getOrElse("--force-specialized-ancestors", "true").toBoolean
    Files.createDirectories(outDir)

    val classFiles: Map[String, Path] = Files.walk(corpus).iterator.asScala
      .filter(p => p.toString.endsWith(".class"))
      .map(p => corpus.relativize(p).toString.stripSuffix(".class") -> p)
      .filter { case (n, _) => only.forall(n.startsWith) }
      .toMap
    val candidates = classFiles.keys.toList.sorted.map(_.replace('/', '.'))

    val results = mutable.ArrayBuffer[ClassResult]()
    val failures = mutable.LinkedHashMap[String, String]()
    val regenerated = mutable.Set[String]()
    val extra = mutable.ArrayBuffer[String]()
    val artifacts = mutable.ArrayBuffer[String]()
    var topLevels = 0

    def runBatch(names: List[String]): Unit = {
      val r = new Relinker(cp, scalacOpts.drop(1), forceSpec)
      try {
        r.relink(names)
        failures ++= r.failures
        val own = r.ownMembers
        for ((cls, bytes) <- r.outputs) {
          // closures in the stub's super constructor call (by-name arguments) are artifacts of the stub
          if (cls.contains("$anonfun$")) artifacts += cls
          else classFiles.get(cls) match {
            case Some(p) =>
              regenerated += cls
              results += Compare.compare(cls, bytes, Files.readAllBytes(p), own.getOrElse(cls, Own(Set.empty, Set.empty)))
            case None => extra += cls
          }
        }
      } catch {
        case t: Throwable if names.size > 1 =>
          val (a, b) = names.splitAt(names.size / 2)
          runBatch(a); runBatch(b)
        case t: Throwable =>
          failures(names.head) = "batch: " + t.toString.take(300)
      }
    }

    // find the top-level names in a throwaway Relinker, then relink in batches
    val probe = new Relinker(cp, scalacOpts.drop(1))
    val tops = candidates.filter(n => try probe.topLevel(n).nonEmpty catch { case t: Throwable => if (sys.props.contains("linkstep.debug")) t.printStackTrace(); false })
    topLevels = tops.size
    val t0 = System.nanoTime()
    tops.grouped(batch).foreach(b => runBatch(b))
    val secs = (System.nanoTime() - t0) / 1e9

    // of the class files not regenerated (local and anonymous classes have no pickle), how many show signs of
    // ancestor-derived members: a `$init$` call, a bridge, a trait setter or super accessor, a mixin forwarder
    val notRegenerated = classFiles.keySet -- regenerated
    val withSigns = notRegenerated.toList.filter { n =>
      val cn = Compare.read(Files.readAllBytes(classFiles(n)))
      val isTrait = (cn.access & scala.tools.asm.Opcodes.ACC_INTERFACE) != 0
      Compare.initCalls(cn).nonEmpty || Compare.members(cn).exists(m => Compare.looksDerived(m, isTrait).exists(_ != "specialized variant of own method"))
    }
    writeReports(outDir, label, classFiles.size, topLevels, results.toList, failures.toMap, extra.toList, notRegenerated, withSigns.size, secs)
  }

  def writeReports(outDir: Path, label: String, nClassFiles: Int, nTop: Int, results: List[ClassResult],
                   failures: Map[String, String], extra: List[String], notRegenerated: Set[String], notRegeneratedWithSigns: Int, secs: Double): Unit = {
    val diffs = results.flatMap(_.diffs)
    Files.write(outDir.resolve(s"$label.tsv"),
      diffs.map(d => s"${d.level}\t${d.kind}\t${d.cls}\t${d.what.replace("\n", "\\n")}").asJava)
    Files.write(outDir.resolve(s"$label-classes.tsv"),
      results.sortBy(_.cls).map(r => s"${r.cls}\t${r.level1Ok}\t${r.level2Ok}\t${r.initCalls.mkString(",")}\t${r.derived.toList.sorted.map { case (k, v) => s"$k=$v" }.mkString(",")}").asJava)

    Files.write(outDir.resolve(s"$label-members.tsv"),
      results.sortBy(_.cls).flatMap(r => r.derivedMembers.map { case (k, m) => s"${r.cls}\t$k\t${m.name}${m.desc}\t${Compare.flags(m.access)}" }).asJava)
    val withDerived = results.filter(_.hasDerived)
    val sb = new StringBuilder
    sb ++= s"corpus $label: $nClassFiles class files, $nTop top-level pickled names, ${"%.1f".format(secs)} s\n"
    sb ++= s"classes regenerated and checked: ${results.size}\n"
    sb ++= s"  with an ancestor-derived member or \\$$init\\$$ call: ${withDerived.size}\n"
    sb ++= s"  of those, level 1 exact (member set, flags, \\$$init\\$$ calls): ${withDerived.count(_.level1Ok)}\n"
    sb ++= s"  of those, level 2 exact (and bodies): ${withDerived.count(_.level2Ok)}\n"
    sb ++= s"  level 1 exact ignoring fresh-name counters: ${withDerived.count(_.level1OkModFresh)}\n"
    sb ++= s"  level 2 exact ignoring fresh-name counters: ${withDerived.count(_.level2OkModFresh)}\n"
    sb ++= s"regenerated classes with no counterpart in the corpus: ${extra.size}\n"
    sb ++= s"top-level names that failed: ${failures.size}\n"
    failures.groupBy(_._2.takeWhile(_ != ':')).toList.sortBy(-_._2.size).foreach { case (k, v) =>
      sb ++= s"  ${v.size} in $k, e.g. ${v.head._1}: ${v.head._2}\n"
    }
    val nr = notRegenerated.toList
    def nrKind(n: String) = {
      val simple = n.substring(n.lastIndexOf('/') + 1)
      if (simple.matches(""".*\$\$anon\$.*""")) "anonymous class"
      else if (simple.matches(""".*\$mc.*\$sp\$?""")) "specialized subclass"
      else if (simple.matches(""".*\$[^$]+\$\d+\$?""") || simple.matches(""".*\$\d+\$?""")) "local class"
      else "other"
    }
    sb ++= s"corpus class files not regenerated: ${nr.size}, with signs of ancestor-derived members: $notRegeneratedWithSigns\n"
    nr.groupBy(nrKind).toList.sortBy(-_._2.size).foreach { case (k, v) => sb ++= s"  $k: ${v.size}, e.g. ${v.sorted.head}\n" }
    sb ++= "\nderived members by kind (regenerated): \n"
    results.flatMap(_.derived).groupMapReduce(_._1)(_._2)(_ + _).toList.sortBy(-_._2).foreach { case (k, v) => sb ++= f"  $v%6d $k\n" }
    sb ++= "\nmismatches by level and kind (diffs, classes):\n"
    diffs.groupBy(d => (d.level, if (d.freshName) "fresh name: " + d.kind else d.kind)).toList.sortBy(_._1).foreach { case ((l, k), ds) =>
      sb ++= f"  L$l $k%-28s ${ds.size}%6d ${ds.map(_.cls).distinct.size}%6d   e.g. ${ds.head.cls}: ${ds.head.what.linesIterator.next()}\n"
    }
    Files.write(outDir.resolve(s"$label-summary.txt"), sb.toString.getBytes)
    print(sb)
  }
}
