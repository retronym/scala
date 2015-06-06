package scala.tools.nsc.interpreter

class PresentationCompilerAPI(interpreter: IMain) {
  /**
   * Index the top-level classes in the the provided packages for the `not found: Class` error
   * message.
   */
  def scanPackagesForClassNotFoundMessage(packages: Set[String]): Unit = {
    interpreter.global.platform.addForceIndexPackages(packages)
  }
}
