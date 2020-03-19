package scala.tools.nsc.async

trait AsyncStateMachine[F, R] {
  protected def state$async_=(i: Int): Unit
  protected def state$async: Int
  protected def completeFailure(t: Throwable): Unit
  protected def completeSuccess(value: AnyRef): Unit
  protected def onComplete(f: F): Unit
  protected def getCompleted(f: F): R
  protected def tryGet(tr: R): AnyRef
}
