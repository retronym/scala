package scala.reflect.internal.ijmodel

/**
 * Mirrors psi/types/api/designator/ScProjectionType.scala's `actualSubst`: the
 * substitutor accompanying a projection's resolved element — the projected
 * element's signature substitutor from the PREFIX's class (MixinNodes hops),
 * plus the prefix designator's own type-argument instantiation.
 */
trait ScProjectionTypes { self: IjTypeSystem =>
  import symbolTable._

  object ScProjectionType {
    def actualSubst(projected: Type, element: Symbol): ScSubstitutor = {
      if ((projected eq NoPrefix) || (projected eq NoType)) return ScSubstitutor.empty
      val preCls = extractClass(projected)
      if (!preCls.isClass || preCls.isPackageClass) return ScSubstitutor.empty
      val sigSubst = TypeDefinitionMembers.getSignatures(preCls)
        .find(_.namedElement == element).map(_.substitutor).getOrElse(ScSubstitutor.empty)
      val preArgs = projected.widen.dealias match {
        case TypeRef(_, pc, as) if as.nonEmpty =>
          new ScSubstitutor(Vector(TypeParamSubstitution(pc.typeParams.zip(as).toMap)))
        case _ => ScSubstitutor.empty
      }
      sigSubst.followed(preArgs)
    }
  }
}
