package scala.reflect.internal

import dev.retronym.tracerbullet.TracerBullet
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.reflect.internal.ijmodel.{EngineMode, IjTypeSystem}
import scala.tools.nsc.symtab.SymbolTableForUnitTesting

/**
 * Runtime call-tree traces for the SAME member lookup computed two ways, printed
 * to the console (stderr) via tracer-bullet's self-attach API:
 *
 *   1. the IntelliJ replica  — everything under scala.reflect.internal.ijmodel
 *      (BaseProcessor dispatch, MixinNodes signatures, ScProjectionType
 *      substitutor minting, ScSubstitutor.recursiveUpdateImpl,
 *      ThisTypeSubstitution walks);
 *   2. the scalac oracle     — Type#memberType / Type#asSeenFrom and the
 *      AsSeenFromMap internals.
 *
 * Pure API mode: the tracer is attached FIRST (before the type system
 * initializes, so every class of interest is woven at load time), no pointcut
 * gates — sessions are scoped manually with `tracer.gated(label) { ... }`,
 * which also keeps warm-up and initialization out of the output.
 *
 * Reading the two trees side by side is the point: the IJ tree shows layered
 * substitutor CONSTRUCTION (signatures composed, chains prepended) followed by
 * chain application; the scalac tree shows a single asSeenFrom traversal doing
 * the same work by function application — the composition-reified vs
 * composition-applied contrast this model exists to study.
 *
 * Requires ~/.tracer-bullet/tracer-bullet-agent.jar (wired into Test/unmanagedJars
 * by build.sbt) and -Djdk.attach.allowAttachSelf=true (in junit's javaOptions).
 */
@RunWith(classOf[JUnit4])
class IjModelTraceTest {

  object ij extends IjTypeSystem {
    val symbolTable: SymbolTableForUnitTesting = new SymbolTableForUnitTesting
  }

  @Test def traceLookupAndOracle(): Unit = {
    import ij._
    import ij.symbolTable._
    import ijFixtures._
    implicit val mode: EngineMode = EngineMode.ProgressConsumed

    val ceThis = ThisType(symbolOf[CEg[_]])

    // Warm up BEFORE attaching, on a DIFFERENT (pre, member) pair than the traced
    // calls: forces classloading, symbol completion and lazy infos, without
    // populating scalac's per-(pre, sym) caches (MethodSymbol.typeAsMemberOf) for
    // the lookups we trace below.
    ReferenceExpressionResolver.resolvePath(ceThis, "en", "values")
    val enPathWarm = singleType(ceThis, ceThis.member(TermName("en")))
    enPathWarm.memberType(enPathWarm.member(TermName("values")))

    val tracer = TracerBullet.attach()
      .trace(
        // the IntelliJ replica, whole package
        "scala.reflect.internal.ijmodel..*",
        // the scalac counterpart: memberType -> asSeenFrom -> AsSeenFromMap
        "scala.reflect.internal.Types$Type#memberType",
        "scala.reflect.internal.Types$Type#asSeenFrom",
        "scala.reflect.internal.tpe.TypeMaps$AsSeenFromMap#*")
      .exclude("*.toString", "*.render", "*.nameString")
      // render substitutor chains as their update lists, not @hashcode
      .render(classOf[ij.ScSubstitutor], (s: ij.ScSubstitutor) => s"[${s.render}]")
      .maxNodes(3000)
      .maxRenderedLength(120)
      .printOnComplete()
      // also dump each session to disk for perusal (text + collapsible html);
      // the junit fork's working directory is the repo root
      .textOutput(java.nio.file.Paths.get(".tracer-bullet"))
      .outputFormats("text", "html")
      .start()

    try {
      // The traced lookup: values' ValueSet result, then .toL on it — a pair the
      // warm-up did NOT touch, so both engines do their full work under trace.
      val vsPath = {
        val (_, vsType, _) = ReferenceExpressionResolver.resolve(enPathWarm, "values")
        vsType
      }

      println("\n════ 1. IntelliJ replica: resolve(CEg.this.en.ValueSet, \"toL\") ════")
      val body1: Runnable = () => {
        val (_, ijType, _) = ReferenceExpressionResolver.resolve(vsPath, "toL")
        println(s"     -> $ijType")
      }
      tracer.gated("intellij-replica resolve(en.ValueSet, toL)", body1)

      println("\n════ 2. scalac oracle: en.ValueSet.memberType(toL) ════")
      val body2: Runnable = () => {
        val oracle = vsPath.memberType(vsPath.member(TermName("toL"))).resultType
        println(s"     -> $oracle")
      }
      tracer.gated("scalac memberType(en.ValueSet, toL)", body2)

      println("\n(match stats: " + tracer.matchStats + ")")
    } finally tracer.close()
  }
}
