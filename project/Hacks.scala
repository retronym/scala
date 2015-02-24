// we used package `sbt` so we have access to sbt-internal APIs...
package sbt
import sbt.Def.Initialize
import CrossVersion.{ binarySbtVersion, binaryScalaVersion, partialVersion }
import sbt.Defaults._
import sbt.Keys._
import sbt.Classpaths._

object Hacks {
  // this is copy of `updateTask` from
  // https://github.com/sbt/sbt/blob/0.13/main/src/main/scala/sbt/Defaults.scala#L1310
  // the only modification we've made here is to
  // depend on `ivyModule in update` instead of just `ivyModule`
  def hackedUpdateTask: Initialize[Task[UpdateReport]] = Def.task {
    val depsUpdated = transitiveUpdate.value.exists(!_.stats.cached)
    val isRoot = executionRoots.value contains resolvedScoped.value
    val s = streams.value
    val scalaProvider = appConfiguration.value.provider.scalaProvider

    // Only substitute unmanaged jars for managed jars when the major.minor parts of the versions the same for:
    //   the resolved Scala version and the scalaHome version: compatible (weakly- no qualifier checked)
    //   the resolved Scala version and the declared scalaVersion: assume the user intended scalaHome to override anything with scalaVersion
    def subUnmanaged(subVersion: String, jars: Seq[File]) = (sv: String) =>
      (partialVersion(sv), partialVersion(subVersion), partialVersion(scalaVersion.value)) match {
        case (Some(res), Some(sh), _) if res == sh     => jars
        case (Some(res), _, Some(decl)) if res == decl => jars
        case _                                         => Nil
      }
    val subScalaJars: String => Seq[File] = Defaults.unmanagedScalaInstanceOnly.value match {
      case Some(si) => subUnmanaged(si.version, si.jars)
      case None     => sv => if (scalaProvider.version == sv) scalaProvider.jars else Nil
    }
    val transform: UpdateReport => UpdateReport = r => substituteScalaFiles(scalaOrganization.value, r)(subScalaJars)
    val uwConfig = (unresolvedWarningConfiguration in update).value
    val show = Reference.display(thisProjectRef.value)
    val st = state.value
    val logicalClock = LogicalClock(st.hashCode)
    val depDir = dependencyCacheDirectory.value
    cachedUpdate(s.cacheDirectory / updateCacheName.value, show, (ivyModule in update).value, updateConfiguration.value, transform,
      skip = (skip in update).value, force = isRoot, depsUpdated = depsUpdated,
      uwConfig = uwConfig, logicalClock = logicalClock, depDir = Some(depDir), log = s.log)
  }
}
