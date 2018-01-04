package scala.tools.nsc.jpms

import java.io.{File, IOException}

class BaseJpmsTest {
  def runNegativeTest(testFiles: Array[String], commandLine: String,
                      expectedFailureOutOutputString: String, expectedFailureErrOutputString: String,
                      shouldFlushOutputDirectory: Boolean): Unit = ()

  @throws[Exception]
  protected def checkDisassembledClassFile(fileName: String, className: String, expectedOutput: String) = ()

  protected def runConformTest(testFiles: Array[String], commandLine: String, expectedFailureOutOutputString: String, expectedFailureErrOutputString: String, shouldFlushOutputDirectory: Boolean): Unit = {
  }

}

object BaseJpmsTest {
}

abstract class JavacCompiler {
  def compliance: Int
  @throws[IOException]
  @throws[InterruptedException]
  def compile(outputDir: File, commandLine: String, testFileNames: Array[String], log: StringBuffer): Long = 0
}
object ClassFileConstants {
  def JDK9: Int = 9
}
