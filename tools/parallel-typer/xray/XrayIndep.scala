// A baseline for the parallel typer's scaling: k independent serial compilations of a corpus at once in one JVM, each
// with its own Global on its own thread, sharing the heap, the collector and the machine's caches and nothing else.
// What k of them gain over one after another is what the machine allows a perfectly parallel compiler of this code.
//
//   XrayIndep <args file> <warmup> <rounds> <k>...
//
// Per k: the wall time of a batch of k (median of the rounds), a compilation's own wall and CPU time within it, and
// the throughput against k compilations one after another.
import java.lang.management.ManagementFactory

object XrayIndep {
  def main(args: Array[String]): Unit = {
    val files = XrayBench.fileArgs(args(0))
    val warmup = args(1).toInt; val rounds = args(2).toInt; val ks = args.drop(3).map(_.toInt)
    val mx = ManagementFactory.getThreadMXBean
    for (_ <- 1 to warmup) XrayBench.compile("serial", files)
    def median(xs: Seq[Double]) = xs.sorted.apply(xs.length / 2)
    var one = 0.0
    for (k <- ks) {
      val batches = for (_ <- 1 to rounds) yield {
        System.gc()
        val res = new java.util.concurrent.ConcurrentLinkedQueue[(Double, Double)]
        val ts = (1 to k).map(i => new Thread(null, () => {
          val c0 = mx.getCurrentThreadCpuTime
          val (ms, _) = XrayBench.compile("serial", files)
          res.add((ms, (mx.getCurrentThreadCpuTime - c0) / 1e6))
        }, s"indep-$i", 1L << 28))
        val t0 = System.nanoTime; ts.foreach(_.start()); ts.foreach(_.join())
        val r = res.toArray(Array.empty[(Double, Double)])
        ((System.nanoTime - t0) / 1e6, r.map(_._1).sum / k, r.map(_._2).sum / k)
      }
      val wall = median(batches.map(_._1)); val each = median(batches.map(_._2)); val cpu = median(batches.map(_._3))
      if (k == 1) one = wall
      println(f"k=$k%-2d batch $wall%7.0f ms; each compilation $each%7.0f ms wall, $cpu%7.0f ms CPU" +
        (if (one > 0) f"; throughput ${k * one / wall}%.2fx of one at a time" else ""))
    }
  }
}
