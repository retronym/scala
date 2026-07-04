package scala.reflect.internal.ijmodel

/**
 * Mirrors psi/impl/toplevel/typedef/MixinNodes.scala and TypeDefinitionMembers:
 * a template's member signatures, each carrying the substitutor that re-anchors
 * its declaring class's type params and this-type onto the template.
 *
 * `SuperTypesData.hopSubstitutor` is one inheritance hop (parent as written in
 * the extends clause).  Inherited signatures compose the declaring-side
 * substitutor FIRST and each hop AFTER it, so a hop's replacement is processed
 * by the chain remainder (outer instantiations see inner output — reverse the
 * order and twoHopGenerics' S never resolves).
 */
trait MixinNodes { self: IjTypeSystem =>
  import symbolTable._

  case class Signature(namedElement: Symbol, substitutor: ScSubstitutor)

  object MixinNodes {
    object SuperTypesData {
      /** One inheritance HOP: TypeParamSubstitution for the parent's params as
       *  written, then ThisTypeSubstitution re-anchoring the parent's this onto
       *  the subclass's this. */
      def hopSubstitutor(c: Symbol, parentAsWritten: Type): ScSubstitutor = {
        val psym = parentAsWritten.typeSymbol
        val upds = Vector.newBuilder[Update]
        if (psym.typeParams.nonEmpty)
          upds += TypeParamSubstitution(psym.typeParams.zip(parentAsWritten.typeArgs).toMap)
        upds += ThisTypeSubstitution(ThisType(c), Some(psym))
        new ScSubstitutor(upds.result())
      }
    }
  }

  object TypeDefinitionMembers {
    /** Own decls (terms AND classes — type members ride the same nodes) with the
     *  empty substitutor; inherited decls with the recursively composed hop
     *  substitutors; linearization-order shadowing by name. */
    def getSignatures(c: Symbol): List[Signature] = {
      val own = c.info.decls.toList.filter(m => m.isTerm || m.isClass).map(Signature(_, ScSubstitutor.empty))
      val inherited = c.info.parents.flatMap { pt =>
        val hop = MixinNodes.SuperTypesData.hopSubstitutor(c, pt)
        getSignatures(pt.typeSymbol).map(s => Signature(s.namedElement, s.substitutor.followed(hop)))
      }
      val seen = collection.mutable.Set.empty[Name]
      (own ++ inherited).filter(s => seen.add(s.namedElement.name))
    }
  }
}
