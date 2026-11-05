package scala.tools.nsc
package typechecker

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

/** The hot reads of the type checker that a concurrent compiler (`-Yparallel-typer`) puts an extra test or an
 *  indirection on, amplified: each op runs over a few thousand real, completed symbols of the standard library.
 *
 *  `universe=serial` is the batch compiler; `concurrent` is a `Global` built for `-Yparallel-typer` (where the
 *  build has it; else the trial is skipped), used from the benchmark thread, as one worker of the parallel typer
 *  uses it. Comparing one build's two universes gives the cost of the concurrent implementations; comparing builds at
 *  `serial` gives the cost of the tests that choose between them.
 */
@BenchmarkMode(Array(Mode.AverageTime))
@Fork(value = 3, jvmArgsAppend = Array("-Xms2G", "-Xmx2G", "-Xss2M"))
@Threads(1)
@Warmup(iterations = 8, time = 1)
@Measurement(iterations = 8, time = 1)
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@State(Scope.Benchmark)
class SerialPathBenchmark {
  @Param(Array("serial"))
  var universe: String = _

  var g: Global = _
  private var syms: Array[AnyRef] = _
  private var classes: Array[AnyRef] = _
  private var tpes: Array[AnyRef] = _

  @Setup(Level.Trial)
  def setup(): Unit = {
    val settings = new Settings()
    settings.usejavacp.value = true
    settings.stopAfter.value = List("typer")
    if (universe == "concurrent") {
      val (ok, residual) = settings.processArgumentString("-Yparallel-typer")
      if (!ok || residual.nonEmpty) throw new IllegalStateException("no -Yparallel-typer in this build")
    }
    val global = new Global(settings)
    g = global
    import global._
    val run = new Run()
    run.compileSources(newSourceFile("class C { def f = List(1).map(_ + 1).toVector; val m = Map(1 -> \"\") }") :: Nil)
    // Owners whose members cover the shapes the typer meets: classes, traits, objects, polymorphic methods, vals
    val owners = List("scala.collection.immutable.List", "scala.collection.immutable.Vector", "scala.collection.immutable.Map",
      "scala.collection.immutable.Seq", "scala.collection.Iterable", "scala.collection.IterableOps", "scala.Option",
      "scala.collection.mutable.ArrayBuffer", "scala.collection.mutable.HashMap", "scala.Predef", "scala.Array",
      "scala.collection.StringOps", "scala.Int", "scala.Function1", "scala.Tuple2", "scala.collection.immutable.Range")
    val ownerSyms = owners.map(n => if (n == "scala.Predef") definitions.PredefModule.moduleClass else rootMirror.getClassByName(TypeName(n)))
    exitingTyper {
      val ms = ownerSyms.flatMap(c => c :: c.info.members.toList)
      ms.foreach { m => m.info; m.typeParams; m.paramss.foreach(_.foreach(_.info)) }
      syms = ms.distinct.toArray[Symbol].asInstanceOf[Array[AnyRef]]
      val cs = ms.flatMap(m => m.info.baseClasses).distinct
      classes = cs.toArray[Symbol].asInstanceOf[Array[AnyRef]]
      tpes = cs.filter(_.typeParams.isEmpty).map(_.tpe_*).toArray[Type].asInstanceOf[Array[AnyRef]]
    }
  }


  /** `Symbol.info` of completed symbols: the completion test on the fast path, and `rawInfo`'s period checks. */
  @Benchmark def info(bh: Blackhole): Unit = {
    val global = g
    import global._
    val ss = syms.asInstanceOf[Array[Symbol]]
    enteringTyper {
      var i = 0
      while (i < ss.length) { bh.consume(ss(i).info); i += 1 }
    }
  }

  /** `enteringPhase`, a phase write and read per symbol, as the transforms and erasure's `transformInfo` do. */
  @Benchmark def enteringPhase(bh: Blackhole): Unit = {
    val global = g
    import global._
    val ss = syms.asInstanceOf[Array[Symbol]]
    val r = currentRun
    val ph1 = r.typerPhase
    val ph2 = r.picklerPhase
    var i = 0
    while (i < ss.length) {
      val s = ss(i)
      bh.consume(global.enteringPhase(if ((i & 1) == 0) ph1 else ph2)(s.info))
      i += 1
    }
  }

  /** `typeParams`, `exists` and `tpe_*`: the other symbol reads with a concurrent branch. */
  @Benchmark def symbolReads(bh: Blackhole): Unit = {
    val global = g
    import global._
    val ss = syms.asInstanceOf[Array[Symbol]]
    enteringTyper {
      var i = 0
      while (i < ss.length) {
        val s = ss(i)
        bh.consume(s.typeParams)
        bh.consume(s.exists)
        if (s.isType) bh.consume(s.tpe_*)
        i += 1
      }
    }
  }

  /** `isSubType` over every pair of a few hundred class types: the recursion guards of the frame. */
  @Benchmark def subTypes(bh: Blackhole): Unit = {
    val global = g
    import global._
    val ts = tpes.asInstanceOf[Array[Type]]
    enteringTyper {
      var i = 0
      while (i < ts.length) {
        var j = 0
        while (j < ts.length) { bh.consume(ts(i) <:< ts(j)); j += 7 }
        i += 1
      }
    }
  }

  /** `baseTypeSeq` and `baseClasses` of class types: stamped caches. */
  @Benchmark def baseTypes(bh: Blackhole): Unit = {
    val global = g
    import global._
    val cs = classes.asInstanceOf[Array[Symbol]]
    enteringTyper {
      var i = 0
      while (i < cs.length) {
        val tp = cs(i).tpe_*
        bh.consume(tp.baseTypeSeq)
        bh.consume(tp.baseClasses)
        i += 1
      }
    }
  }
}
