// Wave-four library campaign: ordinary usage of the library files earlier spikes left unreached,
// as programs of calls checked step by step against obviously-right models.
// usage: LibDiff <family|all> <seed> <seconds>
// Families are registered in Families.all; each generates a program (a vector of ops), runs it on fresh
// instances, and on the first mismatch is shrunk by deleting ops while the signature stays the same.
import scala.collection.{immutable => im, mutable => mu}
import scala.util.Random

final case class Mismatch(sig: String, detail: String) extends RuntimeException(sig, null, false, false)

object H {
  def norm(a: Any): Any = a match {
    case d: Double => "d" + java.lang.Double.doubleToLongBits(d)
    case f: Float => "f" + java.lang.Float.floatToIntBits(f)
    case arr: Array[_] => arr.iterator.map(norm).toList
    case it: Iterator[_] => it.map(norm).toList
    case s: scala.collection.Set[_] => s.iterator.map(norm).toSet
    case m: scala.collection.Map[_, _] => m.iterator.map { case (k, v) => (norm(k), norm(v)) }.toMap
    case it: Iterable[_] => it.iterator.map(norm).toList
    case o: Option[_] => o.map(norm)
    case p: Product if p.productArity > 0 && p.productPrefix.startsWith("Tuple") => "T" :: p.productIterator.map(norm).toList
    case x => x
  }
  def ev(f: => Any): Any = try norm(f) catch {
    case _: IndexOutOfBoundsException => "EX:IndexOutOfBounds"
    case _: NoSuchElementException => "EX:NoSuchElement"
    case e: StackOverflowError => "EX:StackOverflowError"
    case e: Exception => "EX:" + e.getClass.getSimpleName
  }
  def kind(x: Any) = x match { case s: String if s.startsWith("EX:") => s; case _ => "val" }
  var checks = 0L
  /** compare; `fam` and `op` form the signature. */
  def chk(fam: String, op: String)(exp: => Any)(act: => Any): Unit = {
    checks += 1
    val e = ev(exp); val a = ev(act)
    if (e != a) throw Mismatch(s"$fam | $op | ${kind(e)} -> ${kind(a)}", s"expected ${String.valueOf(e).take(400)} got ${String.valueOf(a).take(400)}")
  }
  def check(fam: String, op: String, ok: Boolean, detail: => String): Unit = { checks += 1; if (!ok) throw Mismatch(s"$fam | $op | property", detail.take(600)) }
}

abstract class Fam {
  type P
  def name: String
  def gen(r: Random): Vector[P]
  def run(prog: Vector[P]): Unit // throws Mismatch
  def show(p: P): String = p.toString
}

object LibDiff {
  val sigs = mu.LinkedHashMap.empty[String, (Int, String)]
  @volatile var beat = 0L; @volatile var where = ""
  def runFam(f: Fam, seed: Long, secs: Int): Unit = {
    val master = new Random(seed); val deadline = System.nanoTime() + secs * 1000000000L
    var cases = 0
    while (System.nanoTime() < deadline) {
      val cs = master.nextLong(); cases += 1
      val prog = f.gen(new Random(cs)); where = s"${f.name} case $cases"; beat = System.nanoTime()
      def attempt(p: Vector[f.P]): Option[Mismatch] = try { f.run(p); None } catch {
        case m: Mismatch => Some(m)
        case e: Throwable if !e.isInstanceOf[VirtualMachineError] || e.isInstanceOf[StackOverflowError] => Some(Mismatch(s"${f.name} | harness | ${e.getClass.getSimpleName}", e.toString + " at " + e.getStackTrace.take(4).mkString(" < ")))
      }
      attempt(prog).foreach { m0 =>
        val (n, ex) = sigs.getOrElse(m0.sig, (0, ""))
        if (n == 0) {
          // shrink: delete ops while the signature is unchanged
          var cur = prog; var m = m0; var progress = true; var budget = 400
          while (progress && budget > 0) {
            progress = false
            var i = 0
            while (i < cur.length && budget > 0) {
              val cand = cur.patch(i, Nil, 1); budget -= 1
              attempt(cand) match { case Some(m2) if m2.sig == m0.sig => cur = cand; m = m2; progress = true; case _ => i += 1 }
            }
          }
          val ex1 = s"seed=$seed case=$cases caseSeed=$cs\n    prog: ${cur.map(f.show).mkString("; ")}\n    ${m.detail}"
          println(s"NEW ${m0.sig}\n    $ex1"); Console.flush()
          sigs(m0.sig) = (1, ex1)
        } else sigs(m0.sig) = (n + 1, ex)
      }
    }
    println(s"family ${f.name}: $cases cases in ${secs}s")
  }
  def main(args: Array[String]): Unit = {
    val fam = args(0); val seed = args(1).toLong; val secs = args(2).toInt
    val t = new Thread(() => while (true) { Thread.sleep(2000); if (beat != 0 && System.nanoTime() - beat > 10000000000L) { println(s"HANG at $where"); Console.flush(); Runtime.getRuntime.halt(3) } })
    t.setDaemon(true); t.start()
    val fams = if (fam == "all") Families.all else Families.all.filter(f => fam.split(",").contains(f.name))
    val per = if (fam == "all") secs / fams.size max 1 else secs / fams.size max 1
    fams.foreach(f => runFam(f, seed, per))
    println(s"==== checks=${H.checks} signatures=${sigs.size}"); Excl.counts.foreach { case (k, c) => println(s"excluded $c: $k") }
    sigs.foreach { case (s, (n, ex)) => println(s"$n\t$s\n    $ex") }
  }
}
