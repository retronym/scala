package partest

object TrapExit {

  private class TrapExitThrowable(val status: Int) extends Throwable {
    override def getMessage: String = throw this
    override def getCause: Throwable = throw this
  }

  def apply[A](action: () => A): Either[Int, A] = {
    val saved = System.getSecurityManager
    System.setSecurityManager(new DelegatingSecurityManager(saved) {
      override def checkExit(status: Int): Unit = throw new TrapExitThrowable(status)
    })
    try {
      Right(action())
    } catch {
      case te: TrapExitThrowable =>
        Left(te.status)
    } finally {
      System.setSecurityManager(saved)
    }
  }
}
