package scala.tools.nsc
package typechecker
package splain

trait SplainErrors { self: Analyzer with SplainFormatting =>
  import global._

  def splainPushNotFound(tree: Tree, param: Symbol): Unit =
    ImplicitErrors.stack
      .headOption
      .map(ImplicitError.notFound(_, tree, ImplicitErrors.nesting)(param))
      .foreach(err => ImplicitErrors.push(err))

  def splainPushOrReportNotFound(tree: Tree, param: Symbol): Option[String] =
    if (splainSettingEnable)
      if (ImplicitErrors.nested) {
        splainPushNotFound(tree, param)
        None
      }
      else pluginsNoImplicitFoundError(param, ImplicitErrors.errors, formatImplicitError(param, ImplicitErrors.errors))
    else None

  def splainPushNonconformantBonds(
    tpe: Type,
    candidate: Tree,
    targs: List[Type],
    tparams: List[Symbol],
    originalError: Option[AbsTypeError],
  ): Unit = {
    val err = ImplicitError.nonconformantBounds(tpe, candidate, ImplicitErrors.nesting)(targs, tparams, originalError)
    ImplicitErrors.push(err)
  }

  def splainPushImplicitSearchFailure(implicitTree: Tree, expectedType: Type, originalError: AbsTypeError): Unit = {
    def pushImpFailure(fun: Tree, args: List[Tree]): Unit = {
      fun.tpe match {
        case PolyType(tparams, restpe) if tparams.nonEmpty && sameLength(tparams, args) =>
          val targs = mapList(args)(treeTpe)
          splainPushNonconformantBonds(expectedType, implicitTree, targs, tparams, Some(originalError))
        case _ => ()
      }
    }
    implicitTree match {
      case TypeApply(fun, args) => pushImpFailure(fun, args)
      case Apply(TypeApply(fun, args), _) => pushImpFailure(fun, args)
    }
  }
}
