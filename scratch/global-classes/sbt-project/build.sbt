// Scratch build to see which subclasses of scala.tools.nsc.Global sbt instantiates in one class loader.
// Uses the instrumented compiler from `publishLocal` of branch instrument-global-subclasses.
ThisBuild / scalaVersion := sys.props.getOrElse("scala.version", "2.13.19-bin-SNAPSHOT")
ThisBuild / resolvers += Resolver.mavenLocal

lazy val macros = project.settings(
  libraryDependencies += scalaOrganization.value % "scala-reflect" % scalaVersion.value,
)

lazy val app = project.dependsOn(macros)
lazy val app2 = project.dependsOn(macros)
