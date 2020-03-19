package scala.tools.nsc.async

import java.util.Objects
import scala.language.experimental.macros

import scala.annotation.compileTimeOnly
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.reflect.macros.blackbox
import scala.tools.nsc.transform.async.StateAssigner
import scala.util.{Failure, Success, Try}

object AsyncAsMacro {
  def async[T](body: T)(implicit executionContext: ExecutionContext): Future[T] = macro impl
  @compileTimeOnly("`await` must be enclosed in `async`")
  def await[T](completableFuture: Future[T]): T = ???

  def impl(c: blackbox.Context)(body: c.Tree)(executionContext: c.Tree): c.Tree = {
    import c.universe._
    val awaitSym = typeOf[AsyncAsMacro.type].decl(TermName("await"))
    def mark(t: DefDef): Tree = {
      c.internal.markForAsyncTransform(body.pos, t, awaitSym, Map.empty)
    }
    val name = TypeName("stateMachine$$async_" + body.pos.line)
    q"""
      final class $name extends _root_.scala.tools.nsc.async.AsyncAsMacroStateMachine($executionContext) {
        ${mark(q"""override def apply(tr$$async: _root_.scala.util.Try[_root_.scala.AnyRef]) = ${body}""")}
      }
      new $name().start().asInstanceOf[${c.macroApplication.tpe}]
    """
  }
}

abstract class AsyncAsMacroStateMachine(execContext: ExecutionContext) extends AsyncStateMachine[Future[AnyRef], Try[AnyRef]] with Function1[Try[AnyRef], Unit] {
  Objects.requireNonNull(execContext)

  private val result$async: Promise[AnyRef] = Promise[AnyRef]();

  // FSM translated method
  def apply(tr$async: Try[AnyRef]): Unit

  // Required methods
  protected var state$async: Int = StateAssigner.Initial
  protected def completeFailure(t: Throwable): Unit = result$async.complete(Failure(t))
  protected def completeSuccess(value: AnyRef): Unit = result$async.complete(Success(value))
  protected def onComplete(f: Future[AnyRef]): Unit = f.onComplete(this)(execContext)
  protected def getCompleted(f: Future[AnyRef]): Try[AnyRef] = {
    if (f.isCompleted) {
      f.value.get
    } else null
  }
  protected def tryGet(tr: Try[AnyRef]): AnyRef = tr match {
    case Success(value) =>
      value.asInstanceOf[AnyRef]
    case Failure(throwable) =>
      completeFailure(throwable)
      this // sentinel value to indicate the dispatch loop should exit.
  }
  def start(): Future[AnyRef] = {
    Future.unit.asInstanceOf[Future[AnyRef]].onComplete(this)(execContext)
    result$async.future
  }
}
