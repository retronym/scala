package scala.build

import sbt._, Keys._
import com.typesafe.tools.mima.core._
import com.typesafe.tools.mima.plugin.MimaPlugin, MimaPlugin.autoImport._

object MimaFilters extends AutoPlugin {
  override def trigger = allRequirements

  object autoImport {
    val mimaReferenceVersion = settingKey[Option[String]]("Scala version number to run MiMa against")
  }
  import autoImport._

  override val globalSettings = Seq(
    mimaReferenceVersion := Some("2.13.14"),
  )

  val mimaFilters: Seq[ProblemFilter] = Seq[ProblemFilter](
    // KEEP: the reflect internal API isn't public API
    ProblemFilters.exclude[Problem]("scala.reflect.internal.*"),

    // KEEP: java.util.Enumeration.asIterator only exists in later JDK versions (11 at least).  If you build
    // with JDK 11 and run MiMa it'll complain IteratorWrapper isn't forwards compatible with 2.13.0 - but we
    // don't publish the artifact built with JDK 11 anyways
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.convert.JavaCollectionWrappers#IteratorWrapper.asIterator"),

    // KEEP: when building on a recent JDK, classes implementing `CharSequence` get a mixin forwarder for
    // the `isEmpty` default method that was added in JDK 15
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.Predef#SeqCharSequence.isEmpty"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.Predef#ArrayCharSequence.isEmpty"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.runtime.ArrayCharSequence.isEmpty"),

    // KEEP: make use of CompletionStage#handle to get a better performance than CompletionStage#whenComplete.
    ProblemFilters.exclude[MissingTypesProblem]("scala.concurrent.impl.FutureConvertersImpl$P"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.concurrent.impl.FutureConvertersImpl#P.andThen"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.concurrent.impl.FutureConvertersImpl#P.apply"),
    ProblemFilters.exclude[IncompatibleMethTypeProblem]("scala.concurrent.impl.FutureConvertersImpl#P.andThen"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.concurrent.impl.FutureConvertersImpl#P.accept"),
    ProblemFilters.exclude[IncompatibleMethTypeProblem]("scala.concurrent.impl.FutureConvertersImpl#P.andThen"),

    ProblemFilters.exclude[MissingClassProblem]("scala.collection.convert.impl.AnyChampStepper"),
    ProblemFilters.exclude[MissingClassProblem]("scala.collection.convert.impl.AnyChampStepper$"),
    ProblemFilters.exclude[MissingClassProblem]("scala.collection.convert.impl.ChampStepperBase"),
    ProblemFilters.exclude[MissingClassProblem]("scala.collection.convert.impl.DoubleChampStepper"),
    ProblemFilters.exclude[MissingClassProblem]("scala.collection.convert.impl.DoubleChampStepper$"),
    ProblemFilters.exclude[MissingClassProblem]("scala.collection.convert.impl.IntChampStepper"),
    ProblemFilters.exclude[MissingClassProblem]("scala.collection.convert.impl.IntChampStepper$"),
    ProblemFilters.exclude[MissingClassProblem]("scala.collection.convert.impl.LongChampStepper"),
    ProblemFilters.exclude[MissingClassProblem]("scala.collection.convert.impl.LongChampStepper$"),
    ProblemFilters.exclude[MissingClassProblem]("scala.collection.immutable.BitmapIndexedMapNode"),
    ProblemFilters.exclude[MissingClassProblem]("scala.collection.immutable.BitmapIndexedSetNode"),
    ProblemFilters.exclude[MissingClassProblem]("scala.collection.immutable.ChampBaseIterator"),
    ProblemFilters.exclude[MissingClassProblem]("scala.collection.immutable.ChampBaseReverseIterator"),
    ProblemFilters.exclude[MissingClassProblem]("scala.collection.immutable.HashCollisionMapNode"),
    ProblemFilters.exclude[MissingClassProblem]("scala.collection.immutable.HashCollisionSetNode"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.HashMap.this"),
    ProblemFilters.exclude[MissingClassProblem]("scala.collection.immutable.HashMapBuilder"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.HashSet.this"),
    ProblemFilters.exclude[MissingClassProblem]("scala.collection.immutable.HashSetBuilder"),
    ProblemFilters.exclude[MissingClassProblem]("scala.collection.immutable.MapKeyIterator"),
    ProblemFilters.exclude[MissingClassProblem]("scala.collection.immutable.MapKeyValueTupleHashIterator"),
    ProblemFilters.exclude[MissingClassProblem]("scala.collection.immutable.MapKeyValueTupleIterator"),
    ProblemFilters.exclude[MissingClassProblem]("scala.collection.immutable.MapKeyValueTupleReverseIterator"),
    ProblemFilters.exclude[MissingClassProblem]("scala.collection.immutable.MapNode"),
    ProblemFilters.exclude[MissingClassProblem]("scala.collection.immutable.MapNode$"),
    ProblemFilters.exclude[MissingClassProblem]("scala.collection.immutable.MapNodeRemoveAllSetNodeIterator"),
    ProblemFilters.exclude[MissingClassProblem]("scala.collection.immutable.MapValueIterator"),
    ProblemFilters.exclude[MissingClassProblem]("scala.collection.immutable.Node"),
    ProblemFilters.exclude[MissingClassProblem]("scala.collection.immutable.Node$"),
    ProblemFilters.exclude[MissingClassProblem]("scala.collection.immutable.SetHashIterator"),
    ProblemFilters.exclude[MissingClassProblem]("scala.collection.immutable.SetIterator"),
    ProblemFilters.exclude[MissingClassProblem]("scala.collection.immutable.SetNode"),
    ProblemFilters.exclude[MissingClassProblem]("scala.collection.immutable.SetNode$"),
    ProblemFilters.exclude[MissingClassProblem]("scala.collection.immutable.SetReverseIterator")
  )

  override val buildSettings = Seq(
    mimaFailOnNoPrevious := false, // we opt everything out, knowing we only check library/reflect
  )

  val mimaSettings: Seq[Setting[_]] = Def.settings(
    mimaPreviousArtifacts       := mimaReferenceVersion.value.map(organization.value % name.value % _).toSet,
    mimaCheckDirection          := "backward",
    mimaBinaryIssueFilters     ++= mimaFilters,
//  mimaReportSignatureProblems := true, // TODO: enable
  )
}
