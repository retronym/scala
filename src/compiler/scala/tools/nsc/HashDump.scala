/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc

import java.io.{BufferedWriter, File, FileWriter}
import java.util.concurrent.atomic.AtomicInteger

import scala.collection.mutable.ArrayBuffer
import scala.reflect.internal.util.HashProbe

/** Probe for whether type hash codes are reproducible across JVMs (`-Dscalac.hashdump=<dir>`).
 *
 *  After the typer, records every typed tree and its type, without hashing anything. At the end of the run, before
 *  per-run caches are cleared, writes `<dir>/<pid>/run-<n>.trees.tsv` (one line per typed tree: unit, start, end,
 *  tree class, tree id, type hash, symbol id and hash, type), `run-<n>.uniques.tsv` (every slot of the chunked uniques
 *  table: slot, home slot, hash, type class, type) and `run-<n>.meta`. All hashes are read before any `toString`, so
 *  printing cannot shift the identity hashes the dump reports; the dump's own first-time hashing does shift the next
 *  run's.
 */
object HashDump {
  val dir: String = HashProbe.HASH_DUMP_DIR
  /** `-Dscalac.hashdump.full=<run indices>`: write the full TSVs only for these runs (default all); other runs write
   *  only `meta`, with order-sensitive digests of the tree hashes, symbol hashes and uniques slots. */
  private val fullRuns: Option[Set[Int]] = sys.props.get("scalac.hashdump.full").map(_.split(',').filter(_.nonEmpty).map(_.toInt).toSet)
  // `-Dscalac.hashdump.preload=<classes>`: load these classes up front, so that no tier-4 compile request loads them
  // later on the compiling thread (`Method::load_signature_classes`), at a JIT-timing-dependent point
  if (dir != null) sys.props.get("scalac.hashdump.preload").foreach(_.split(',').filter(_.nonEmpty).foreach(c => Class.forName(c, false, getClass.getClassLoader)))
  private val runCounter = new AtomicInteger
  @volatile private[nsc] var current: HashDump = null
  /** Record a fingerprint labelled `name` in the current run's `phases` line; for locating hashes within a phase. */
  def mark(name: String): Unit = if (fine) { val c = current; if (c != null) c.afterPhase(name) }
  /** `-Dscalac.hashdump.fine=true`: also record fingerprints within the backend (per unit, method and class). */
  private val fine: Boolean = java.lang.Boolean.getBoolean("scalac.hashdump.fine")
  /** `-Dscalac.hashdump.lite=true`: record no trees; write only `meta` with fingerprints and the uniques table's digest
   *  and probe statistics, cheap enough to leave on in a timed benchmark. */
  private val lite: Boolean = java.lang.Boolean.getBoolean("scalac.hashdump.lite")
  private def pid = ProcessHandle.current().pid()
  private def fingerprint(): Int = System.identityHashCode(new Object)
  private def gcCount(): Long = { var n = 0L; java.lang.management.ManagementFactory.getGarbageCollectorMXBeans.forEach(b => n += b.getCollectionCount); n }
  private def uptime(): Long = java.lang.management.ManagementFactory.getRuntimeMXBean.getUptime
  private def clip(s: String) = { val t = s.replace('\t', ' ').replace('\n', ' '); if (t.length > 160) t.substring(0, 160) else t }
}

final class HashDump(val global: Global) {
  import global._
  import HashDump._

  private final class Rec(val unit: String, val start: Int, val end: Int, val cls: String, val id: Int, val tpe: Type, val sym: Symbol)
  private val recs = ArrayBuffer[Rec]()
  val startFingerprint: Int = fingerprint()
  HashDump.current = this
  private val startNanos = System.nanoTime()
  private val startGc = gcCount()
  private val startUptime = uptime()

  private val phaseFps = new StringBuilder
  /** Called after every phase; with `-XX:hashCode=3` the fingerprint is the global hash counter, which locates extra hashes. */
  def afterPhase(name: String): Unit = phaseFps.append(name).append('=').append(fingerprint()).append(' ')
  private val phaseSizes = new StringBuilder
  /** Uniques size and symbol count after each phase. */
  def phaseSize(name: String): Unit = phaseSizes.append(name).append('=').append(howManyUniqueTypes).append('/').append(getCurrentSymbolIdCount).append(' ')

  def afterTyper(): Unit = if (!lite) {
    for (unit <- currentRun.units) {
      val path = unit.source.file.path
      unit.body.foreach { t =>
        if (t.tpe != null) {
          val p = t.pos
          val (s, e) = if (p.isRange) (p.start, p.end) else if (p.isDefined) (p.point, p.point) else (-1, -1)
          recs += new Rec(path, s, e, t.getClass.getSimpleName, t.id, t.tpe, if (t.hasSymbolField) t.symbol else null)
        }
      }
    }
  }

  def finish(): Unit = {
    val n = runCounter.getAndIncrement()
    val out = new File(new File(dir), pid.toString)
    out.mkdirs()
    // 1. hashes first
    val treeHashes = new Array[Int](recs.length)
    val symHashes = new Array[Int](recs.length)
    var i = 0
    while (i < recs.length) {
      treeHashes(i) = recs(i).tpe.hashCode
      val s = recs(i).sym
      symHashes(i) = if (s == null) 0 else s.hashCode
      i += 1
    }
    val slots = ArrayBuffer[(Int, Int, Int, Type)]()
    val cu = chunkedUniquesForProbe
    // probe statistics: total and maximum displacement of an element from its home slot (linear probing)
    var dispSum = 0L; var dispMax = 0; var liteDigest = 0; var liteCount = 0
    if (cu != null) {
      val cap = cu.capacity
      cu.foreachSlot { (slot, home, h, tp) =>
        val d = (slot - home + cap) % cap
        dispSum += d; if (d > dispMax) dispMax = d
        if (lite) { liteDigest = scala.util.hashing.MurmurHash3.mix(scala.util.hashing.MurmurHash3.mix(liteDigest, slot), h); liteCount += 1 }
        else slots += ((slot, home, h, tp))
      }
    }
    val endFingerprint = fingerprint()
    val endGc = gcCount()
    import scala.util.hashing.MurmurHash3.{mix, finalizeHash}
    var td = 0; var sd = 0; var ud = 0
    i = 0
    while (i < recs.length) { td = mix(td, treeHashes(i)); sd = mix(sd, symHashes(i)); i += 1 }
    for ((slot, _, h, _) <- slots) ud = mix(mix(ud, slot), h)
    td = finalizeHash(td, recs.length); sd = finalizeHash(sd, recs.length)
    ud = if (lite) finalizeHash(liteDigest, liteCount) else finalizeHash(ud, slots.length)
    val full = !lite && fullRuns.forall(_.contains(n))
    // 2. then strings
    def w(name: String)(body: BufferedWriter => Unit): Unit = {
      val bw = new BufferedWriter(new FileWriter(new File(out, f"run-$n%03d.$name")), 1 << 16)
      try body(bw) finally bw.close()
    }
    def str(tp: Type): String = try clip(exitingTyper(tp.toString)) catch { case ex: Throwable => "<" + ex.getClass.getSimpleName + ">" }
    if (full) w("trees.tsv") { bw =>
      i = 0
      while (i < recs.length) {
        val r = recs(i)
        val symS = if (r.sym == null || r.sym == NoSymbol) "-\t0" else s"${r.sym.id}\t${symHashes(i)}"
        bw.write(s"${r.unit}\t${r.start}\t${r.end}\t${r.cls}\t${r.id}\t${treeHashes(i)}\t$symS\t${str(r.tpe)}\n")
        i += 1
      }
    }
    if (full) w("uniques.tsv") { bw =>
      for ((slot, home, h, tp) <- slots)
        bw.write(s"$slot\t$home\t$h\t${if (tp == null) "null" else tp.getClass.getSimpleName}\t${if (tp == null) "" else str(tp)}\n")
    }
    w("meta") { bw =>
      bw.write(s"run\t$n\npid\t$pid\nthread\t${Thread.currentThread.getName}\nsymbolHashById\t${HashProbe.SYMBOL_HASH_BY_ID}\n" +
        s"startFingerprint\t$startFingerprint\nendFingerprint\t$endFingerprint\ntrees\t${recs.length}\nuniques\t${if (lite) liteCount else slots.length}\n" +
        s"probeDispSum\t$dispSum\nprobeDispMax\t$dispMax\nuniquesCapacity\t${if (cu == null) 0 else cu.capacity}\nphaseSizes\t$phaseSizes\ntreeDigest\t$td\nsymDigest\t$sd\nuniquesDigest\t$ud\nphases\t$phaseFps\nsymbols\t$getCurrentSymbolIdCount\nnanos\t${System.nanoTime() - startNanos}\n" +
        s"threadId\t${Thread.currentThread.threadId}\nstartUptimeMs\t$startUptime\nendUptimeMs\t${uptime()}\nstartGc\t$startGc\nendGc\t$endGc\n")
    }
    recs.clear()
    HashDump.current = null
  }
}
