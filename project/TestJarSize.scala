package scala.build

import sbt._, Keys._

object TestJarSize {
  private val libraryJarMaxKiB: Long = 5700L
  private val reflectJarMaxKiB: Long = 3600L

  val testJarSizeImpl: Def.Initialize[Task[Unit]] = Def.task {
    testJarSize1("library", libraryJarMaxKiB).value
    testJarSize1("reflect", reflectJarMaxKiB).value
  }

  private def testJarSize1(projectId: String, maxKiB: Long): Def.Initialize[Task[Unit]] = Def.task {
    val jar = (packageBin in Compile in LocalProject(projectId)).value
    val actualSize = jar.length()
    if (actualSize > maxKiB * 1024) {
      fail(s"The $projectId jar is over the max of $maxKiB KiB: $actualSize bytes.")
    }
  }

  private def fail(message: String): Nothing = {
    val fail = new MessageOnlyException(message)
    fail.setStackTrace(new Array[StackTraceElement](0))
    throw fail
  }
}
