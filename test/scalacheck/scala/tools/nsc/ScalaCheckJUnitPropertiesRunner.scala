package scala.tools.nsc

import org.junit.runner.Description
import org.scalacheck._
import org.scalacheck.{Test => SchkTest}
import org.junit.runner.notification.{Failure, RunNotifier}
import org.scalacheck.Prop.Result
import java.lang.{Boolean, Throwable}

import org.junit.runner
import org.scalacheck.util.Pretty

/**
  * This a JUnit runner that allows to run ScalaCheck properties (created into an object that implements
  * Properties) as part of a JUnit test suite. Each property will be counted as a failure or passed test
  * by JUnit.
  *
  * Properties are written in the exact same way as pure ScalaCheck; the only aifference is that the test suite class
  * needs to be annotated with @RunWith[classOf[ScalaCheckJUnitPropertiesRunner]] so that JUnit knows how to run
  * the tests
  */
class ScalaCheckJUnitPropertiesRunner(suiteClass: java.lang.Class[Properties]) extends org.junit.runner.Runner {

  private val properties = suiteClass.newInstance

  lazy val getDescription = Description.createSuiteDescription(properties.name)

  // Our custom test callback, used to keep JUnit's runner updated about test progress
  class CustomTestCallback(notifier:RunNotifier, desc: Description) extends Test.TestCallback {
    // TODO: is it even possible to obtain the correct stack trace? ScalaCheck doesn't throw Exceptions for property failures!
    def failure = new Failure(desc, new Throwable("ScalaCheck property did not hold true"))

    /** Called whenever a property has finished testing */
    override def onTestResult(name: String, res: Test.Result) = {
      def pretty = Pretty.prettyTestRes(res).apply(Pretty.defaultParams)
      res.status match {
        case Test.Passed => {} // Test passed, nothing to do
        case Test.Proved(_) => {} // Test passed, nothing to do
        case Test.Exhausted =>
          notifier.fireTestFailure(new Failure(desc, new Throwable(pretty)))
        case Test.PropException(args, throwable, labels) =>
          notifier.fireTestFailure(new Failure(desc, throwable))
        case Test.Failed(args, labels) =>
            notifier.fireTestFailure(new Failure(desc, new Throwable(pretty)))
      }
    }
  }

  // we'll use this one to report status to the console, and we'll chain it with our custom reporter
  val consoleReporter = new org.scalacheck.util.ConsoleReporter(2)

  def run(notifier: RunNotifier) {
    properties.properties.foreach {
      case (desc, prop) => {
        val desc1 = desc.stripPrefix(properties.name + ".")
        val descObj = Description.createTestDescription(suiteClass, desc1)

        notifier.fireTestStarted(descObj)
        SchkTest.check(org.scalacheck.Test.Parameters.default.withTestCallback(new CustomTestCallback(notifier, descObj)), prop)
        notifier.fireTestFinished(descObj)
      }
    }
  }

  override def testCount() = properties.properties.size
}