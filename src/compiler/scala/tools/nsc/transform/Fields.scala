/*  NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author
 */

package scala.tools.nsc
package transform

import scala.annotation.tailrec
import symtab.Flags._


/** Synthesize accessors and field for each (strict) val owned by a trait.
  *
  * For traits:
  *
  *   - Namers translates a definition `val x = rhs` into a getter `def x = rhs` -- no underlying field is created.
  *   - This phase synthesizes accessors and fields for any vals mixed into a non-trait class.
  *   - During erasure, AddInterfaces will move the rhs to an assignment in the template body.
  *     We need to maintain the connection between getter and rhs until then, so specialization can duplicate as needed.
  *   - Constructors moves the statements in the template into the constructor,
  *     which means it will initialize the fields defined in this template (and execute the corresponding side effects).
  *
  * Runs before erasure (to get bridges), and thus before lambdalift/flatten, so that nested functions/definitions must be considered.
  *
  * TODO:
  *   - remove backwards compatibility hacks to complete migration to Java 8-encoding of traits
  *   - minimize introduction of new flag bits?
  *   - ...
  *
  * In the future, would like to get closer to dotty, which lifts a val's RHS (a similar thing is done for template-level statements)
  * to a method `$_initialize_$1$x` instead of a block, which is used in the constructor to initialize the val.
  * This makes for a nice unification of strict and lazy vals, in that the RHS is lifted to a method for both,
  * with the corresponding compute method called at the appropriate time.)
  *
  * This only reduces the required number of methods per field declaration in traits,
  * if we encode the name (and place in initialisation order) of the field
  * in the name of its initializing method, to allow separate compilation.
  * (The name mangling must include ordering, and thus complicate incremental compilation:
  *  ideally, we'd avoid renumbering unchanged methods, but that would result in
  *  different bytecode between clean recompiles and incremental ones).
  *
  * In the even longer term (Scala 3?), I agree with @DarkDimius that it would make sense
  * to hide the difference between strict and lazy vals. All vals are lazy,
  * but the memoization overhead is removed when we statically know they are forced during initialiation.
  * We could still expose the low-level field semantics through `private[this] val`s.
  *
  * In any case, the current behavior of overriding vals is pretty surprising.
  * An overridden val's side-effect is still performed.
  * The only change due to overriding is that its value is never written to the field
  * (the overridden val's value is, of course, stored in the field in addition to its side-effect being performed).
  */
abstract class Fields extends InfoTransform with ast.TreeDSL with TypingTransformers {

  import global._
  import definitions._

  /** the following two members override abstract members in Transform */
  val phaseName: String = "fields"

  protected def newTransformer(unit: CompilationUnit): Transformer = new FieldsTransformer(unit)
  override def transformInfo(sym: Symbol, tp: Type): Type =
    if (sym.isJavaDefined || sym.isClass && (sym hasFlag SYNTHESIZE_IMPL_IN_SUBCLASS)) tp
    else {
      // only synthesize members once! TODO: don't overload this flag...
      if (sym.isClass) sym setFlag SYNTHESIZE_IMPL_IN_SUBCLASS
      synthFieldsAndAccessors(tp)
    }


  // we leave lazy vars/accessors and early-init vals alone for now
  private def excludedAccessorOrFieldByFlags(statSym: Symbol): Boolean = statSym hasFlag LAZY | PRESUPER

  // used for internal communication between info and tree transform of this phase -- not pickled, not in initialflags
  // TODO: reuse MIXEDIN for NEEDS_TREES
  override def phaseNewFlags: Long = NEEDS_TREES | OVERRIDDEN_TRAIT_SETTER | MIXEDIN_ACCESSOR // TODO: | lateDEFERRED ?

  private final val OVERRIDDEN_TRAIT_SETTER = TRANS_FLAG

  final val TRAIT_SETTER_FLAGS = NEEDS_TREES | DEFERRED

  private def accessorImplementedInSubclass(accessor: Symbol) = accessor hasAllFlags (ACCESSOR | SYNTHESIZE_IMPL_IN_SUBCLASS)

  private def concreteOrSynthImpl(sym: Symbol): Boolean = !(sym hasFlag DEFERRED) || (sym hasFlag SYNTHESIZE_IMPL_IN_SUBCLASS)

  private def setTraitAccessorFlags(accessor: Symbol): Unit =
    accessor setFlag lateDEFERRED | SYNTHESIZE_IMPL_IN_SUBCLASS

  private def setClonedTraitSetterFlags(clazz: Symbol, correspondingGetter: Symbol, cloneInSubclass: Symbol): Unit = {
    val overridden = isOverriddenAccessor(correspondingGetter, clazz)
    if (overridden) cloneInSubclass setFlag OVERRIDDEN_TRAIT_SETTER
    else if (correspondingGetter.isEffectivelyFinal) cloneInSubclass setFlag FINAL
  }

  // TODO: add MIXEDIN (see e.g., `accessed` on `Symbol`)
  private def setMixedinAccessorFlags(orig: Symbol, cloneInSubclass: Symbol): Unit =
    cloneInSubclass setFlag MIXEDIN_ACCESSOR | OVERRIDE | NEEDS_TREES resetFlag DEFERRED | lateDEFERRED | SYNTHESIZE_IMPL_IN_SUBCLASS

  private def setFieldFlags(accessor: Symbol, fieldInSubclass: TermSymbol): Unit =
    fieldInSubclass setFlag ( NEEDS_TREES |
          PrivateLocal
        | (accessor getFlag MUTABLE | LAZY)
        | (if (accessor hasFlag STABLE) 0 else MUTABLE)
      )


  def checkAndClearOverridden(setter: Symbol) = checkAndClear(OVERRIDDEN_TRAIT_SETTER)(setter)
  def checkAndClearNeedsTrees(setter: Symbol) = checkAndClear(NEEDS_TREES)(setter)
  def checkAndClear(flag: Long)(sym: Symbol) =
    sym.hasFlag(flag) match {
      case overridden =>
        sym resetFlag flag
        overridden
    }


  private def isOverriddenAccessor(member: Symbol, site: Symbol): Boolean = {
    val pre = site.thisType
    @tailrec def loop (bcs: List[Symbol]): Boolean = {
//      println(s"checking ${bcs.head} for member overriding $member (of ${member.owner})")
      bcs.nonEmpty && bcs.head != member.owner && (matchingAccessor(pre, member, bcs.head) != NoSymbol || loop(bcs.tail))
    }

    member.exists && loop(site.info.baseClasses)
  }


  def matchingAccessor(pre: Type, member: Symbol, clazz: Symbol) =  {
    val res = member.matchingSymbol(clazz, pre) filter (sym => (sym hasFlag ACCESSOR) && concreteOrSynthImpl(sym))
//    if (res != NoSymbol) println(s"matching accessor for $member in $clazz = $res (under $pre)")
//    else println(s"no matching accessor for $member in $clazz (under $pre) among ${clazz.info.decls}")
    res
  }



  class FieldMemoization(accessorOrField: Symbol, site: Symbol) {
    val tp = fieldTypeOfAccessorIn(accessorOrField, site.thisType)
    // not stored, no side-effect
    val pureConstant = tp.isInstanceOf[ConstantType]

    // if !stored, may still have a side-effect
    // (currently not distinguished -- used to think we could drop unit-typed vals,
    //  but the memory model cares about writes to unit-typed fields)
    val stored = !pureConstant  // || isUnitType(tp))
  }

  private def fieldTypeForGetterIn(getter: Symbol, pre: Type): Type = getter.info.finalResultType.asSeenFrom(pre, getter.owner)
  private def fieldTypeForSetterIn(setter: Symbol, pre: Type): Type = setter.info.paramTypes.head.asSeenFrom(pre, setter.owner)

  // TODO: is there a more elegant way?
  def fieldTypeOfAccessorIn(accessor: Symbol, pre: Type) =
    if (accessor.isSetter) fieldTypeForSetterIn(accessor, pre)
    else fieldTypeForGetterIn(accessor, pre)


  // Constant/unit typed vals are not memoized (their value is so cheap it doesn't make sense to store it in a field)
  // for a unit-typed getter, we perform the effect at the appropriate time (constructor for eager ones, lzyCompute for lazy),
  // and have the getter just return Unit (who does that!?)
  // NOTE: this only considers type, filter on flags first!
  def fieldMemoizationIn(accessorOrField: Symbol, site: Symbol) = new FieldMemoization(accessorOrField, site)



  private object synthFieldsAndAccessors extends TypeMap {
    private def newTraitSetter(getter: Symbol, clazz: Symbol) = {
      // Add setter for an immutable, memoizing getter
      // (can't emit during namers because we don't yet know whether it's going to be memoized or not)
      val setterFlags = (getter.flags & ~(STABLE | PrivateLocal | OVERRIDE | IMPLICIT | FINAL)) | MUTABLE | ACCESSOR | TRAIT_SETTER_FLAGS
      val setterName  = nme.expandedSetterName(getter.name.setterName, clazz)
      val setter      = clazz.newMethod(setterName, getter.pos.focus, setterFlags)
      val fieldTp     = fieldTypeForGetterIn(getter, clazz.thisType)
      // println(s"newTraitSetter in $clazz for $getter = $setterName : $fieldTp")

      setter setInfo MethodType(List(setter.newSyntheticValueParam(fieldTp)), UnitTpe)
      setter
    }

    def apply(tp0: Type): Type = mapOver(tp0) match {
      // TODO: make less destructive (name changes, decl additions, flag setting --
      // none of this is actually undone when travelling back in time using atPhase)
      case tp@ClassInfoType(parents, decls, clazz) if clazz.isTrait =>
        val newSetters = collection.mutable.ListBuffer[Symbol]()
        val origDecls = decls.toList

        // strict, memoized accessors will receive an implementation in first real class to extend this trait
        origDecls.foreach { accessor => if (accessor hasFlag ACCESSOR) {
          val fieldMemoization = fieldMemoizationIn(accessor, clazz)
          // check flags before calling makeNotPrivate
          val accessorUnderConsideration = !(accessor hasFlag (DEFERRED | LAZY))

          // destructively mangle accessor's name (which may cause rehashing of decls), also sets flags
          if (accessor hasFlag PRIVATE) accessor makeNotPrivate clazz

          // Need to mark as notPROTECTED, so that it's carried over to the synthesized member in subclasses,
          // since the trait member will receive this flag later in ExplicitOuter, but the synthetic subclass member will not.
          // If we don't add notPROTECTED to the synthesized one, the member will not be seen as overriding the trait member.
          // Therefore, addForwarders's call to membersBasedOnFlags would see the deferred member in the trait,
          // instead of the concrete (desired) one in the class
          // TODO: encapsulate as makeNotProtected, similar to makeNotPrivate (also do moduleClass, e.g.)
          if (accessor hasFlag PROTECTED) accessor setFlag notPROTECTED

          // must not reset LOCAL, as we must maintain protected[this]ness to allow that variance hole
          // (not sure why this only problem only arose when we started setting the notPROTECTED flag)

          // derive trait setter after calling makeNotPrivate (so that names are mangled consistently)
          if (accessorUnderConsideration && fieldMemoization.stored) {
            setTraitAccessorFlags(accessor)

            if (accessor hasFlag STABLE) // TODO: check isGetter?
              newSetters += newTraitSetter(accessor, clazz)
          } else if (fieldMemoization.pureConstant) setTraitAccessorFlags(accessor) // TODO: remove when we no longer care about producing identical bytecode
        }}

        if (newSetters nonEmpty) {
//          println(s"newSetters for $clazz = $newSetters")
          val newDecls = newScope
          origDecls  foreach newDecls.enter
          newSetters foreach newDecls.enter
          ClassInfoType(parents, newDecls, clazz)
        } else tp

      // mix in fields & accessors for all mixed in traits

      case tp@ClassInfoType(parents, oldDecls, clazz) if !clazz.isPackageClass =>
        val site = clazz.thisType
        // TODO (1): improve logic below, which is used to avoid mixing in anything that would result in an error in refchecks
        // (a reason to run after refchecks? we should run before pickler, though, I think, so that the synthesized stats are pickled)

        val accessorsMaybeNeedingImpl = clazz.mixinClasses.flatMap { mixin =>
          // afterOwnPhase, so traits receive trait setters for vals
          afterOwnPhase { mixin.info }.decls.toList.filter(accessorImplementedInSubclass)
        }

//        println(s"mixing in for $clazz: $accessorsMaybeNeedingImpl from ${clazz.mixinClasses}")

        // TODO: setter conflicts?
        def accessorConflictsExistingVal(accessor: Symbol): Boolean = {
          val existingGetter = oldDecls.lookup(accessor.name.getterName)
//          println(s"$existingGetter from $accessor to ${accessor.name.getterName}")
          val tp = fieldTypeOfAccessorIn(accessor, site)
          (existingGetter ne NoSymbol) && (tp matches (site memberInfo existingGetter)) // !existingGetter.isDeferred && -- see (3)
        }

        // mixin field accessors --
        // invariant: (accessorsMaybeNeedingImpl, mixedInAccessorAndFields).zipped.forall(case (acc, clone :: _) => `clone` is clone of `acc` case _ => true)
        val mixedInAccessorAndFields = accessorsMaybeNeedingImpl map { accessor =>
          def cloneAccessor() = {
            val clonedAccessor = (accessor cloneSymbol clazz) setPos clazz.pos
            setMixedinAccessorFlags(accessor, clonedAccessor)

            if (clonedAccessor.isGetter)
              clonedAccessor setAnnotations (clonedAccessor.annotations filter AnnotationInfo.mkFilter(GetterTargetClass, defaultRetention = false))

            // if we don't cloneInfo, method argument symbols are shared between trait and subclasses --> lambalift proxy crash
            // TODO: use derive symbol variant?
            //            println(s"cloning accessor $accessor to $clazz / $clonedInfo -> $relativeInfo")
            clonedAccessor setInfo ((clazz.thisType memberType accessor) cloneInfo clonedAccessor) // accessor.info.cloneInfo(clonedAccessor).asSeenFrom(clazz.thisType, accessor.owner)
          }

          // when considering whether to mix in the trait setter, forget about conflicts -- they will be reported for the getter
          // a trait setter for an overridden val will receive a unit body in the tree transform
          if (nme.isTraitSetterName(accessor.name)) {
            val getter = accessor.getterIn(accessor.owner)
            val clone = cloneAccessor()

            clone filterAnnotations (ai => !ai.matches(TraitSetterAnnotationClass)) // only supposed to be set in trait

            setClonedTraitSetterFlags(clazz, getter, clone)
//            println(s"mixed in trait setter ${clone.defString}")

            List(clone)
          }
          // avoid creating early errors in case of conflicts (wait until refchecks);
          // also, skip overridden accessors contributed by supertraits (only act on the last overriding one)
          else if (accessorConflictsExistingVal(accessor) || isOverriddenAccessor(accessor, clazz)) Nil
          else if (accessor.isGetter && fieldMemoizationIn(accessor, clazz).stored) {
            // add field if needed
            val field = clazz.newValue(accessor.localName, accessor.pos) setInfo fieldTypeForGetterIn(accessor, clazz.thisType)

            setFieldFlags(accessor, field)

            // filter getter's annotations to exclude those only meant for the field
            // we must keep them around long enough to see them here, though, when we create the field
            field setAnnotations (accessor.annotations filter AnnotationInfo.mkFilter(FieldTargetClass, defaultRetention = true))

            List(cloneAccessor(), field)
          } else List(cloneAccessor())
        }

//        println(s"new decls for $clazz: $mixedInAccessorAndFields")

        // omit fields that are not memoized, retain all other members
        def omittableField(sym: Symbol) = sym.isValue && !sym.isMethod && !fieldMemoizationIn(sym, clazz).stored

        val newDecls =
          if (mixedInAccessorAndFields.isEmpty) oldDecls.filterNot(omittableField)
          else {  // must not alter `decls` directly
            // compute subst from accessors to corresponding clonedAccessors in types in newDecls
            val (_origs, _mixedins) = (accessorsMaybeNeedingImpl, mixedInAccessorAndFields).zipped.collect {
              case (traitAccessor, mixedin :: _) => (traitAccessor, mixedin)
            }.unzip

            val (origs, mixedins) = (_origs.toList, _mixedins.toList)

            val newDecls = newScope
            val enterAndSubst = { sym: Symbol => newDecls enter sym.substInfo(origs, mixedins) }

            oldDecls foreach { d => if (!omittableField(d)) enterAndSubst(d) }
            mixedInAccessorAndFields foreach { _ foreach enterAndSubst }

            newDecls
          }

//        println(s"new decls: $newDecls")

        if (newDecls eq oldDecls) tp
        else ClassInfoType(parents, newDecls, clazz)

      case tp => tp
    }
  }


  class FieldsTransformer(unit: CompilationUnit) extends TypingTransformer(unit) {
    def mkTypedUnit(pos: Position) = localTyper.typedPos(pos)(CODE.UNIT)
    def deriveUnitDef(stat: Tree)  = deriveDefDef(stat)(_ => mkTypedUnit(stat.pos))

    // synth trees for accessors/fields and trait setters when they are mixed into a class
    def fieldsAndAccessors(templateSym: Symbol): List[ValOrDefDef] = {
      val clazz = templateSym.owner
      def fieldAccess(accessor: Symbol): Option[Tree] = {
        val fieldName = accessor.localName
        val field = clazz.info.decl(fieldName)
        // The `None` result denotes an error, but we defer to refchecks to report it.
        // This is the result of overriding a val with a def, so that no field is found in the subclass.
        if (field.exists) Some(Select(This(clazz), field))
        else None
      }

      val accessorsAndFieldsNeedingTrees = clazz.info.decls.toList.filter(checkAndClearNeedsTrees)

      def getterBody(getter: Symbol): Option[Tree] = {
        val fieldMemoization = fieldMemoizationIn(getter, clazz)
        if (fieldMemoization.pureConstant) Some(gen.mkAttributedQualifier(fieldMemoization.tp)) // TODO: drop when we no longer care about producing identical bytecode
        else fieldAccess(getter)
      }

//      println(s"accessorsAndFieldsNeedingTrees for $templateSym: $accessorsAndFieldsNeedingTrees")
      def setterBody(setter: Symbol): Option[Tree] = {
        // trait setter in trait
        if (clazz.isTrait) Some(EmptyTree)
        // trait setter for overridden val in class
        else if (checkAndClearOverridden(setter)) Some(mkTypedUnit(setter.pos))
        // trait val/var setter mixed into class
        else fieldAccess(setter) map (fieldSel => Assign(fieldSel, Ident(setter.firstParam)))
      }

      def mkAccessor(accessor: Symbol)(body: Tree) = localTyper.typedPos(accessor.pos)(DefDef(accessor, body)).asInstanceOf[DefDef]

      accessorsAndFieldsNeedingTrees flatMap {
        case setter if setter.isSetter                  => setterBody(setter) map mkAccessor(setter)
        case getter if getter.isAccessor                => getterBody(getter) map mkAccessor(getter)
        case field  if field.isValue && !field.isMethod => Some(localTyper.typedPos(field.pos)(ValDef(field)).asInstanceOf[ValDef])
        case _ => None
      }
    }

    def rhsAtOwner(stat: ValOrDefDef, newOwner: Symbol): Tree =
      atOwner(newOwner)(super.transform(stat.rhs.changeOwner(stat.symbol -> newOwner)))

    private def transformStat(exprOwner: Symbol)(stat: Tree): List[Tree] = {
      val clazz = currentOwner
      val statSym = stat.symbol

//       println(s"transformStat $statSym in ${exprOwner.ownerChain}")
      //        currentRun.trackerFactory.snapshot()

      /*
        For traits, the getter has the val's RHS, which is already constant-folded. There is no valdef.
        For classes, we still have the classic scheme of private[this] valdef + getter & setter that read/assign to the field.

        There are two axes: (1) is there a side-effect to the val (2) does the val need storage?
        For a ConstantType, both answers are "no". (For a unit-typed field, there's a side-effect, but no storage needed.)

        All others (getter for trait field, valdef for class field) have their rhs moved to an initialization statement.
        Trait accessors for stored fields are made abstract (there can be no field in a trait).
        (In some future version, accessors for non-stored, but effectful fields,
         would receive a constant rhs, as the effect is performed by the initialization statement.
         We could do this for unit-typed fields, but have chosen not to for backwards compatibility.)
       */
      stat match {
        // TODO: consolidate with ValDef case
        case stat@DefDef(_, _, _, _, _, rhs) if (statSym hasFlag ACCESSOR) && !excludedAccessorOrFieldByFlags(statSym) =>
          /* TODO: defer replacing ConstantTyped tree by the corresponding constant until erasure
             (until then, trees should not be constant-folded -- only their type tracks the resulting constant)
             TODO: also remove ACCESSOR flag since there won't be an underlying field to access?
          */
          def statInlinedConstantRhs =
            if (clazz.isTrait) stat // we've already done this for traits.. the asymmetry will be solved by the above todo
            else deriveDefDef(stat)(_ => gen.mkAttributedQualifier(rhs.tpe))

          if (rhs ne EmptyTree) {
            val fieldMemoization = fieldMemoizationIn(statSym, clazz)

            // if we decide to have non-stored fields with initialization effects, the stat's RHS should be replaced by unit
            // if (!fieldMemoization.stored) deriveUnitDef(stat) else stat

            if (fieldMemoization.pureConstant) statInlinedConstantRhs :: Nil
            else super.transform(stat) :: Nil
          } else {
            stat :: Nil
          }

        case stat@ValDef(mods, _, _, rhs) if !excludedAccessorOrFieldByFlags(statSym) =>
          if (rhs ne EmptyTree) {
            val fieldMemoization = fieldMemoizationIn(statSym, clazz)

            // drop the val for (a) constant (pure & not-stored) and (b) not-stored (but still effectful) fields
            if (fieldMemoization.pureConstant) Nil // (a)
            else super.transform(stat) :: Nil // if (fieldMemoization.stored)
//            else rhsAtOwner(transformStat, exprOwner) :: Nil // (b) -- not used currently
          } else {
            stat :: Nil
          }

        case tree => List (
          if (exprOwner != currentOwner && tree.isTerm) atOwner(exprOwner)(super.transform(tree))
          else super.transform(tree)
        )
      }
    }


    override def transformStats(stats: List[Tree], exprOwner: Symbol): List[Tree] =
      if (!currentOwner.isClass || currentOwner.isPackageClass || currentOwner.isInterface) super.transformStats(stats, exprOwner)
      else afterOwnPhase {
        fieldsAndAccessors(exprOwner) ++ (stats flatMap transformStat(exprOwner)) // TODO use thicket encoding of multi-tree transformStat?
      }
  }
}
