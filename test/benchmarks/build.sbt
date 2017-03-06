scalaHome in ThisBuild := Some(file("../../build/pack"))
scalaVersion in ThisBuild := "2.11.8"
scalacOptions in ThisBuild ++= Seq("-feature", "-Yopt:l:classpath")
publishArtifact in (Compile, packageDoc) in ThisBuild := false

lazy val root = (project in file(".")).
  enablePlugins(JmhPlugin).
  settings(
    name := "test-benchmarks",
    version := "0.0.1",
	libraryDependencies += "org.openjdk.jol" % "jol-core" % "0.4"
  )

// dummy project to bring the right classpath into the `compile` configuration
// so that sbt-native-packager can create a standalone package.
//
// $ sbt packager/universal:packageBin
// % unzip target/packager/target/universal/packager-0.1-SNAPSHOT.zip
// % java -classpath './packager-0.1-SNAPSHOT/lib/*' org.openjdk.jmh.Main ...
lazy val packager = (project in file("target/packager")).
  dependsOn(root % "jmh->compile").
  enablePlugins(JavaAppPackaging).
  enablePlugins(JmhPlugin)