scalaHome := Some(file("../../build/pack"))
scalaVersion := "2.12.1-dev"
scalacOptions ++= Seq("-feature", "-opt:l:inline", "-opt-inline-from:**")

val extraScalaLibs = List("scala-compiler", "scala-reflect")
lazy val root = (project in file(".")).
  enablePlugins(JmhPlugin).
  settings(
    name := "test-benchmarks",
    version := "0.0.1",
    libraryDependencies += "org.openjdk.jol" % "jol-core" % "0.6",
    libraryDependencies ++= {
      scalaHome.value match {
        case Some(home) => Nil
        case _ =>
          extraScalaLibs.map(x => "org.scala-lang" % x % scalaVersion.value)
      }
    },
    unmanagedJars in Compile ++= {
      scalaHome.value match {
        case Some(home) =>
          extraScalaLibs.map(x => home / (x + ".jar"))
        case _ => Nil
      }
    }
  )
