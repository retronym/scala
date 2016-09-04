class UndefinedBehaviorError(message: String, cause: Throwable) extends java.lang.Error(message, cause) with scala.util.control.ControlThrowable {
   override def fillInStackTrace(): Throwable =super[Error].fillInStackTrace()
}
