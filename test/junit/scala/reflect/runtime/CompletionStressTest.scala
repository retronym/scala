package scala.reflect.runtime

import java.io.File
import java.lang.management.ManagementFactory
import java.util.concurrent.{ConcurrentHashMap, CountDownLatch, TimeUnit}

import org.junit.Assert._
import org.junit.Test

import scala.jdk.CollectionConverters._
import scala.util.Random

/** Threads completing the same symbols of a fresh runtime universe, in different orders, must
 *  observe what one thread observes alone, in some order.
 *
 *  "In some order" because a single thread's observations already depend on the order in which it
 *  completes classes (see `oneThreadIsOrderIndependent`). The oracle for a class is the set of what
 *  sequential runs in several orders observe, and a concurrent observation must be one of them.
 *
 *  `-Dscala.reflect.stress.iterations=N` (default 2), `-Dscala.reflect.stress.threads=N`
 *  (default 8), `-Dscala.reflect.stress.classes=N` (default 150) and
 *  `-Dscala.reflect.stress.seed=N` scale it up for a soak.
 */
class CompletionStressTest {
  import CompletionStressTest._

  @Test def concurrentCompletionAgreesWithOneThread(): Unit = {
    val names = classNames.take(classCount)
    val seed = Integer.getInteger("scala.reflect.stress.seed", Random.nextInt())
    val rnd = new Random(seed)
    val oracle = scala.collection.mutable.Map.empty[String, Set[String]].withDefaultValue(Set.empty)
    def addToOracle(order: List[String]): Unit =
      for ((n, o) <- observeAll(names, order)) oracle(n) += o
    addToOracle(names)
    addToOracle(names.reverse)
    for (_ <- 1 to oracleOrders) addToOracle(rnd.shuffle(names))
    for (i <- 1 to iterations) {
      val orders = List.fill(threadCount)(rnd.shuffle(names))
      val failures = runConcurrently(names, orders)
      if (failures.nonEmpty) fail(s"iteration $i (seed $seed, $threadCount threads, ${names.size} classes):\n${failures.take(5).mkString("\n\n")}")
      for (observed <- lastObservations) {
        val diffs = names.filter(n => !oracle(n).contains(observed.get(n)))
        // A miss may be an order the oracle has not tried yet: run each thread's order alone.
        if (diffs.nonEmpty) orders.foreach(addToOracle)
        val stillDiffs = diffs.filter(n => !oracle(n).contains(observed.get(n)))
        for (n <- stillDiffs.headOption)
          fail(s"iteration $i (seed $seed): ${stillDiffs.size} classes differ from every sequential order tried, first $n\n--- sequential (${oracle(n).size} variants)\n${oracle(n).mkString("\n--- or\n")}\n--- concurrent\n${observed.get(n)}")
      }
    }
  }

  // The single-threaded oracle must not depend on the order the classes are completed in;
  // otherwise a difference under concurrency says nothing.
  @Test def oneThreadIsOrderIndependent(): Unit = {
    val names = classNames.take(classCount)
    val a = observeAll(names, names)
    val b = observeAll(names, new Random(1).shuffle(names))
    val diffs = names.filter(n => a(n) != b(n))
    for (n <- diffs.headOption)
      fail(s"${diffs.size} classes depend on completion order (${diffs.mkString(", ")}), first $n\n--- sorted order\n${a(n)}\n--- shuffled order\n${b(n)}")
  }

  private var lastObservations: List[java.util.Map[String, String]] = Nil

  private def runConcurrently(names: List[String], orders: List[List[String]]): List[String] = {
    val u = new JavaUniverse
    val m = u.runtimeMirror(getClass.getClassLoader)
    val start = new CountDownLatch(1)
    val failures = new java.util.concurrent.ConcurrentLinkedQueue[String]
    val results = orders.map(_ => new ConcurrentHashMap[String, String]())
    val threads = orders.zip(results).zipWithIndex.map { case ((order, out), i) =>
      val t = new Thread(() => {
        start.await()
        for (n <- order)
          try out.put(n, observe(u)(m, n))
          catch { case t: Throwable => failures.add(s"$n: ${stackTrace(t)}") }
      }, s"stress-$i")
      t.setDaemon(true)
      t.start()
      t
    }
    start.countDown()
    val deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(120)
    threads.foreach(t => t.join(math.max(1, TimeUnit.NANOSECONDS.toMillis(deadline - System.nanoTime()))))
    if (threads.exists(_.isAlive)) failures.add(s"threads still running after 120 s:\n${threadDump()}")
    lastObservations = results
    failures.asScala.toList
  }

  private def observeAll(names: List[String], order: List[String]): Map[String, String] = {
    val u = new JavaUniverse
    val m = u.runtimeMirror(getClass.getClassLoader)
    order.map(n => n -> observe(u)(m, n)).toMap
  }
}

object CompletionStressTest {
  val iterations: Int = Integer.getInteger("scala.reflect.stress.iterations", 2)
  val threadCount: Int = Integer.getInteger("scala.reflect.stress.threads", 8)
  val classCount: Int = Integer.getInteger("scala.reflect.stress.classes", 150)
  val oracleOrders: Int = Integer.getInteger("scala.reflect.stress.oracleOrders", 4)

  /** What a thread sees of a class: its signature, base classes and declarations. */
  def observe(u: JavaUniverse)(m: u.Mirror, name: String): String = {
    import u._
    val cls = m.staticClass(name)
    // `toType` on a class that is not completed yet answers the unapplied type constructor
    // (`unsafeTypeParams` reads the completer's empty `typeParams`), so complete it first.
    cls.info
    val tpe = cls.toType
    val sb = new java.lang.StringBuilder
    sb.append(cls.typeSignature).append('\n')
    sb.append(tpe.baseClasses.map(_.fullName).mkString(", ")).append('\n')
    for (d <- tpe.decls.sorted)
      sb.append(d.name).append(": ").append(d.typeSignatureIn(tpe)).append('\n')
    sb.toString
  }

  /** Top-level classes of the Scala collections, from the library on the class path. */
  lazy val classNames: List[String] = {
    val root = new File(classOf[List[_]].getProtectionDomain.getCodeSource.getLocation.toURI)
    val prefix = "scala/collection/"
    val entries: List[String] =
      if (root.isDirectory) {
        val base = root.toPath
        java.nio.file.Files.walk(base.resolve(prefix)).iterator.asScala
          .map(p => base.relativize(p).toString.replace(File.separatorChar, '/')).toList
      } else {
        val jar = new java.util.jar.JarFile(root)
        try jar.entries.asScala.map(_.getName).filter(_.startsWith(prefix)).toList finally jar.close()
      }
    entries.filter(e => e.endsWith(".class") && !e.contains("$"))
      .map(_.stripSuffix(".class").replace('/', '.'))
      .sorted
  }

  def stackTrace(t: Throwable): String = {
    val sw = new java.io.StringWriter
    t.printStackTrace(new java.io.PrintWriter(sw))
    sw.toString.linesIterator.take(25).mkString("\n")
  }

  /** Full stacks, with the monitors each frame holds; `ThreadInfo.toString` stops at 8 frames. */
  def threadDump(): String =
    ManagementFactory.getThreadMXBean.dumpAllThreads(true, true)
      .filter(_.getThreadName.startsWith("stress-"))
      .map { ti =>
        val locked = ti.getLockedMonitors.groupBy(_.getLockedStackDepth)
        val frames = ti.getStackTrace.zipWithIndex.map { case (f, i) =>
          s"\tat $f" + locked.getOrElse(i, Array.empty).map(m => s"\n\t- locked $m").mkString
        }
        s""""${ti.getThreadName}" ${ti.getThreadState} on ${ti.getLockName} owned by ${ti.getLockOwnerName}\n""" +
          frames.filter(f => f.contains("scala.reflect") || f.contains("- locked")).mkString("\n") +
          ti.getLockedSynchronizers.map(s => s"\n\t- holds $s").mkString
      }.mkString("\n\n")
}
