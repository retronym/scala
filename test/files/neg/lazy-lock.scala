object NoParams {
  def __lazyLock(a: Int) = new {}
  lazy val x = ""
}

object NoOverload {
  def __lazyLock(a: Int) = new {}
  def __lazyLock = new {}
  lazy val x = ""
}

object NoPrimitive {
  def __lazyLock = 0
  lazy val x = ""
}
