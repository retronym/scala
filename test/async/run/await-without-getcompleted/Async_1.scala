package nogetcompleted

import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.language.experimental.macros
import scala.reflect.macros.blackbox
import scala.util.{Failure, Success, Try}

// A front end whose state machine has no `getCompleted`, which is optional.
object Async {
  def async[T](body: T)(implicit executionContext: ExecutionContext): Future[T] = macro impl
  def await[T](future: Future[T]): T = ???

  def impl(c: blackbox.Context)(body: c.Tree)(executionContext: c.Tree): c.Tree = {
    import c.universe._
    val awaitSym = typeOf[Async.type].decl(TermName("await"))
    def mark(t: DefDef): Tree = c.internal.markForAsyncTransform(c.internal.enclosingOwner, t, awaitSym, Map.empty)
    val name = TypeName("stateMachine$async")
    q"""
      final class $name extends _root_.nogetcompleted.StateMachine($executionContext) {
        ${mark(q"""override def apply(tr$$async: _root_.scala.util.Try[_root_.scala.AnyRef]) = ${body}""")}
      }
      new $name().start().asInstanceOf[${c.macroApplication.tpe}]
    """
  }
}

abstract class StateMachine(execContext: ExecutionContext) extends Function1[Try[AnyRef], Unit] {
  private val result$async: Promise[AnyRef] = Promise[AnyRef]()
  private[this] var state$async: Int = 0

  def apply(tr$async: Try[AnyRef]): Unit

  protected def state: Int = state$async
  protected def state_=(s: Int): Unit = state$async = s
  protected def completeFailure(t: Throwable): Unit = result$async.complete(Failure(t))
  protected def completeSuccess(value: AnyRef): Unit = result$async.complete(Success(value))
  protected def onComplete(f: Future[AnyRef]): Unit = f.onComplete(this)(execContext)
  protected def tryGet(tr: Try[AnyRef]): AnyRef = tr match {
    case Success(value) => value
    case Failure(throwable) =>
      completeFailure(throwable)
      this
  }
  def start(): Future[AnyRef] = {
    Future.unit.asInstanceOf[Future[AnyRef]].onComplete(this)(execContext)
    result$async.future
  }
}
