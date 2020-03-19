package scala.tools.nsc
package async

import java.util.Objects
import java.util.concurrent.{CompletableFuture, Executor}
import java.util.function.BiConsumer

import scala.language.experimental.macros
import scala.annotation.compileTimeOnly
import scala.reflect.macros.blackbox
import scala.tools.nsc.transform.async.StateAssigner
import scala.util.{Failure, Success, Try}

object CompletableFutureAwait {
  def async[T](executor: Executor)(body: T): CompletableFuture[T] = macro impl
  @compileTimeOnly("`await` must be enclosed in `async`")
  def await[T](completableFuture: CompletableFuture[T]): T = ???
  def impl(c: blackbox.Context)(executor: c.Tree)(body: c.Tree): c.Tree = {
    import c.universe._
    def mark(t: DefDef): Tree = {
      val g = c.universe.asInstanceOf[Global]
      g.async.markForAsyncTransform(
        c.macroApplication.pos.asInstanceOf[g.Position],
        t.asInstanceOf[g.DefDef],
        typeOf[CompletableFutureAwait.type].decl(TermName("await")).asInstanceOf[g.Symbol],
        Map.empty
      ).asInstanceOf[c.Tree]
    }
    val name = TypeName("stateMachine$$async_" + body.pos.line)
    q"""
        {
          final class $name extends _root_.scala.tools.nsc.async.CompletableFutureStateMachine($executor) {
            ${mark(q"""override def apply(tr$$async: R[_root_.scala.AnyRef]) = ${body}""")}
          }
          new $name().start().asInstanceOf[${c.macroApplication.tpe}]
        }
    """

  }
}

abstract class CompletableFutureStateMachine(executor: Executor) extends Runnable with BiConsumer[AnyRef, Throwable] {
  Objects.requireNonNull(executor)

  var result$async: CompletableFuture[AnyRef] = new CompletableFuture[AnyRef]();
  var state$async: Int = StateAssigner.Initial
  def accept(value: AnyRef, throwable: Throwable): Unit = {
    this(if (throwable != null) Failure(throwable) else Success(value))
  }
  def apply(tr$async: R[AnyRef]): Unit
  def run(): Unit = {
    apply(null)
  }

  type F[A] = CompletableFuture[A]
  type R[A] = Try[A]
  // Adapter methods
  protected def completeFailure(t: Throwable): Unit = result$async.completeExceptionally(t)
  protected def completeSuccess(value: AnyRef): Unit = result$async.complete(value)
  protected def onComplete(f: F[AnyRef]): Unit = f.whenCompleteAsync(this)
  protected def getCompleted(f: F[AnyRef]): R[AnyRef] = {
    try {
      val r = f.getNow(this)
      if (r == this) null
      else Success(r)
    } catch {
      case t: Throwable =>
        Failure(t)
    }
  }
  protected def tryGet(tr: R[AnyRef]): AnyRef = tr match {
    case Success(value) =>
      value.asInstanceOf[AnyRef]
    case Failure(throwable) =>
      result$async.completeExceptionally(throwable)
      this // sentinel value to indicate the dispatch loop should exit.
  }
  def start(): CompletableFuture[AnyRef] = {
    executor.execute(this)
    result$async
  }
}
