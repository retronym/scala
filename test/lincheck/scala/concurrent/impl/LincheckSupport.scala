package scala.concurrent.impl

import org.jetbrains.kotlinx.lincheck.LinCheckerKt
import org.jetbrains.kotlinx.lincheck.strategy.managed.modelchecking.ModelCheckingOptions
import org.jetbrains.kotlinx.lincheck.strategy.stress.StressOptions

import scala.util.{Failure, Success, Try}

/** Shared configuration for the Lincheck tests.
 *
 *  System properties (forwarded from the sbt JVM to the forked test JVM, see `build.sbt`):
 *   - `lincheck.iterations`: number of random scenarios per test (default: per-test, modest)
 *   - `lincheck.invocations`: number of invocations (interleavings) per scenario
 *   - `lincheck.modelChecking=false`: skip the model checking tests (e.g. on a JDK where instrumentation fails)
 */
object LincheckSupport {
  private def intProp(name: String, default: Int): Int = sys.props.get(name).map(_.toInt).getOrElse(default)

  def iterations(default: Int): Int  = intProp("lincheck.iterations", default)
  def invocations(default: Int): Int = intProp("lincheck.invocations", default)
  def modelCheckingEnabled: Boolean  = sys.props.get("lincheck.modelChecking").forall(_.toBoolean)

  def stress(iterations: Int = 20, invocations: Int = 1000): StressOptions =
    new StressOptions()
      .iterations(this.iterations(iterations))
      .invocationsPerIteration(this.invocations(invocations))
      .threads(3)
      .actorsPerThread(3)
      .actorsBefore(2)
      .actorsAfter(2)

  // Smaller scenarios than for stress: the number of interleavings explodes with the scenario size.
  def modelChecking(iterations: Int = 8, invocations: Int = 200): ModelCheckingOptions =
    new ModelCheckingOptions()
      .iterations(this.iterations(iterations))
      .invocationsPerIteration(this.invocations(invocations))
      .threads(3)
      .actorsPerThread(2)
      .actorsBefore(1)
      .actorsAfter(1)

  def check(options: StressOptions, testClass: Class[_]): Unit = timed("stress", testClass)(LinCheckerKt.check(options, testClass))
  def check(options: ModelCheckingOptions, testClass: Class[_]): Unit = {
    org.junit.Assume.assumeTrue("model checking disabled via -Dlincheck.modelChecking=false", modelCheckingEnabled)
    timed("model checking", testClass)(LinCheckerKt.check(options, testClass))
  }
  private def timed(what: String, testClass: Class[_])(body: => Unit): Unit = {
    val start = System.nanoTime()
    try body
    finally println(f"${testClass.getSimpleName} $what: ${(System.nanoTime() - start) / 1e9}%.1fs")
  }

  /** A failure carrying a comparable identity; stackless to keep the stress runs cheap. */
  final class TestException(val code: Int) extends RuntimeException(s"TestException($code)", null, false, false)

  /** A comparable rendering of a promise value: `Left(code)` for a `TestException` failure, `Right(v)` for a success. */
  def encode(t: Try[Int]): Either[Int, Int] = t match {
    case Success(v)                    => Right(v)
    case Failure(e: TestException)     => Left(e.code)
    case Failure(e)                    => throw new AssertionError(s"unexpected failure $e", e)
  }
  def encode(v: Option[Try[Int]]): Option[Either[Int, Int]] = v.map(encode)

  /** The `private[this]` sentinel `Promise.Noop`, i.e. the state of a promise without callbacks. */
  lazy val Noop: AnyRef = {
    val f = Promise.getClass.getDeclaredFields.find(_.getName.contains("Noop")).get
    f.setAccessible(true)
    f.get(Promise)
  }

  /** Number of callbacks registered directly on `p` (0 if `p` is completed or linked). */
  def numCallbacks(p: Promise.DefaultPromise[_]): Int = {
    def count(cs: AnyRef): Int = cs match {
      case Noop                        => 0
      case m: Promise.ManyCallbacks[_] => 1 + count(m.rest)
      case _: Promise.Transformation[_, _] => 1
      case _                           => 0
    }
    count(p.get())
  }
}
