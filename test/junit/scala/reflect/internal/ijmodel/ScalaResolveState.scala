package scala.reflect.internal.ijmodel

/**
 * Mirrors resolve/ScalaResolveState.scala: the accumulated substitutor plus the
 * `compoundOrThisType` "ugly workaround" flag (set when the ThisType dispatch
 * recurses into a self type; makes processElement SKIP following the element's
 * own substitutor).
 */
trait ScalaResolveStates { self: IjTypeSystem =>

  case class ScalaResolveState(substitutor: ScSubstitutor, compoundOrThisType: Boolean = false) {
    def withSubstitutor(s: ScSubstitutor): ScalaResolveState = copy(substitutor = s)
    def withCompoundOrSelfType: ScalaResolveState = copy(compoundOrThisType = true)
  }

  object ScalaResolveState {
    def empty: ScalaResolveState = ScalaResolveState(ScSubstitutor.empty)
  }
}
