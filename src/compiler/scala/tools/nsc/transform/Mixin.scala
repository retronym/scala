/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author Martin Odersky
 */

package scala.tools.nsc
package transform

import symtab._
import Flags._
import scala.collection.{ mutable, immutable }

abstract class Mixin extends InfoTransform with ast.TreeDSL {
  import global._
  import definitions._
  import CODE._

  /** The name of the phase: */
  val phaseName: String = "mixin"

  /** The phase might set the following new flags: */
  override def phaseNewFlags: Long = lateMODULE | notOVERRIDE

  /** This map contains a binding (class -> info) if
   *  the class with this info at phase mixinPhase has been treated for mixin composition
   */
  private val treatedClassInfos = perRunCaches.newMap[Symbol, Type]() withDefaultValue NoType

  /** Map a lazy, mixedin field accessor to its trait member accessor */
  private val initializer = perRunCaches.newMap[Symbol, Symbol]()

// --------- helper functions -----------------------------------------------

  /** A member of a trait is implemented statically if its implementation after the
   *  mixin transform is in the static implementation module. To be statically
   *  implemented, a member must be a method that belonged to the trait's implementation class
   *  before (i.e. it is not abstract). Not statically implemented are
   *   - non-private modules: these are implemented directly in the mixin composition class
   *     (private modules, on the other hand, are implemented statically, but their
   *      module variable is not. all such private modules are lifted, because
   *      non-lifted private modules have been eliminated in ExplicitOuter)
   *   - field accessors and superaccessors, except for lazy value accessors which become initializer
   *     methods in the impl class (because they can have arbitrary initializers)
   */
  private def isImplementedStatically(sym: Symbol) = (
       sym.owner.isImplClass
    && sym.isMethod
    && (!sym.isModule || sym.hasFlag(PRIVATE | LIFTED))
    && (!(sym hasFlag (ACCESSOR | SUPERACCESSOR)) || sym.isLazy)
  )

  /** A member of a trait is static only if it belongs only to the
   *  implementation class, not the interface, and it is implemented
   *  statically.
   */
  private def isStaticOnly(sym: Symbol) =
    isImplementedStatically(sym) && sym.isImplOnly

  /** A member of a trait is forwarded if it is implemented statically and it
   *  is also visible in the trait's interface. In that case, a forwarder to
   *  the member's static implementation will be added to the class that
   *  inherits the trait.
   */
  private def isForwarded(sym: Symbol) =
    isImplementedStatically(sym) && !sym.isImplOnly

  /** Maps the type of an implementation class to its interface;
   *  maps all other types to themselves.
   */
  private def toInterface(tp: Type): Type =
    enteringMixin(tp.typeSymbol.toInterface).tpe

  /** Maps all parts of this type that refer to implementation classes to
   *  their corresponding interfaces.
   */
  private val toInterfaceMap = new TypeMap {
    def apply(tp: Type): Type = mapOver( tp match {
      case TypeRef(pre, sym, args) if sym.isImplClass =>
        typeRef(pre, enteringMixin(sym.toInterface), args)
      case _ => tp
    })
  }

  /** The implementation class corresponding to a currently compiled interface.
   *  todo: try to use Symbol.implClass instead?
   */
  private def implClass(iface: Symbol) = iface.implClass orElse (erasure implClass iface)

  /** Returns the symbol that is accessed by a super-accessor in a mixin composition.
   *
   *  @param base       The class in which everything is mixed together
   *  @param member     The symbol statically referred to by the superaccessor in the trait
   *  @param mixinClass The mixin class that produced the superaccessor
   */
  private def rebindSuper(base: Symbol, member: Symbol, mixinClass: Symbol): Symbol =
    exitingSpecialize {
      var bcs = base.info.baseClasses.dropWhile(mixinClass != _).tail
      var sym: Symbol = NoSymbol
      debuglog("starting rebindsuper " + base + " " + member + ":" + member.tpe +
            " " + mixinClass + " " + base.info.baseClasses + "/" + bcs)
      while (!bcs.isEmpty && sym == NoSymbol) {
        if (settings.debug) {
          val other = bcs.head.info.nonPrivateDecl(member.name)
          debuglog("rebindsuper " + bcs.head + " " + other + " " + other.tpe +
              " " + other.isDeferred)
        }
        sym = member.matchingSymbol(bcs.head, base.thisType).suchThat(sym => !sym.hasFlag(DEFERRED | BRIDGE))
        bcs = bcs.tail
      }
      sym
    }

// --------- type transformation -----------------------------------------------

  def isConcreteAccessor(member: Symbol) =
    member.hasAccessorFlag && (!member.isDeferred || (member hasFlag lateDEFERRED))

  /** Is member overridden (either directly or via a bridge) in base class sequence `bcs`? */
  def isOverriddenAccessor(member: Symbol, bcs: List[Symbol]): Boolean = beforeOwnPhase {
    def hasOverridingAccessor(clazz: Symbol) = {
      clazz.info.nonPrivateDecl(member.name).alternatives.exists(
        sym =>
          isConcreteAccessor(sym) &&
          !sym.hasFlag(MIXEDIN) &&
          matchesType(sym.tpe, member.tpe, alwaysMatchSimple = true))
    }
    (    bcs.head != member.owner
      && (hasOverridingAccessor(bcs.head) || isOverriddenAccessor(member, bcs.tail))
    )
  }

  /** Add given member to given class, and mark member as mixed-in.
   */
  def addMember(clazz: Symbol, member: Symbol): Symbol = {
    debuglog("new member of " + clazz + ":" + member.defString)
    clazz.info.decls enter member setFlag MIXEDIN
  }
  def cloneAndAddMember(mixinClass: Symbol, mixinMember: Symbol, clazz: Symbol): Symbol =
    addMember(clazz, cloneBeforeErasure(mixinClass, mixinMember, clazz))

  def cloneBeforeErasure(mixinClass: Symbol, mixinMember: Symbol, clazz: Symbol): Symbol = {
    val newSym = enteringErasure {
      // since we used `mixinMember` from the interface that represents the trait that's
      // being mixed in, have to instantiate the interface type params (that may occur in mixinMember's
      // info) as they are seen from the class.  We can't use the member that we get from the
      // implementation class, as it's a clone that was made after erasure, and thus it does not
      // know its info at the beginning of erasure anymore.
      val sym = mixinMember cloneSymbol clazz

      val erasureMap = erasure.erasure(mixinMember)
      val erasedInterfaceInfo: Type = erasureMap(mixinMember.info)
      val specificForwardInfo       = (clazz.thisType baseType mixinClass) memberInfo mixinMember
      val forwarderInfo =
        if (erasureMap(specificForwardInfo) =:= erasedInterfaceInfo)
          specificForwardInfo
        else {
          erasedInterfaceInfo
        }
      // Optimize: no need if mixinClass has no typeparams.
      // !!! JZ Really? What about the effect of abstract types, prefix?
      if (mixinClass.typeParams.isEmpty) sym
      else sym modifyInfo (_ => forwarderInfo)
    }
    newSym
  }

  /** Add getters and setters for all non-module fields of an implementation
   *  class to its interface unless they are already present. This is done
   *  only once per class. The mixedin flag is used to remember whether late
   *  members have been added to an interface.
   *    - lazy fields don't get a setter.
   */
  def addLateInterfaceMembers(clazz: Symbol) {
    if (treatedClassInfos(clazz) != clazz.info) {
      treatedClassInfos(clazz) = clazz.info
      assert(phase == currentRun.mixinPhase, phase)

      /* Create a new getter. Getters are never private or local. They are
       *  always accessors and deferred. */
      def newGetter(field: Symbol): Symbol = {
        // println("creating new getter for "+ field +" : "+ field.info +" at "+ field.locationString+(field hasFlag MUTABLE))
        val newFlags = field.flags & ~PrivateLocal | ACCESSOR | lateDEFERRED | ( if (field.isMutable) 0 else STABLE )
        // TODO preserve pre-erasure info?
        clazz.newMethod(field.getterName, field.pos, newFlags) setInfo MethodType(Nil, field.info)
      }

      /* Create a new setter. Setters are never private or local. They are
       * always accessors and deferred. */
      def newSetter(field: Symbol): Symbol = {
        //println("creating new setter for "+field+field.locationString+(field hasFlag MUTABLE))
        val setterName = field.setterName
        val newFlags   = field.flags & ~PrivateLocal | ACCESSOR | lateDEFERRED
        val setter     = clazz.newMethod(setterName, field.pos, newFlags)
        // TODO preserve pre-erasure info?
        setter setInfo MethodType(setter.newSyntheticValueParams(List(field.info)), UnitTpe)
        if (field.needsExpandedSetterName)
          setter.name = nme.expandedSetterName(setter.name, clazz)

        setter
      }

      clazz.info // make sure info is up to date, so that implClass is set.
      val impl = implClass(clazz) orElse abort("No impl class for " + clazz)

      for (member <- impl.info.decls) {
        if (!member.isMethod && !member.isModule && !member.isModuleVar) {
          assert(member.isTerm && !member.isDeferred, member)
          if (member.getterIn(impl).isPrivate) {
            member.makeNotPrivate(clazz) // this will also make getter&setter not private
          }
          val getter = member.getterIn(clazz)
          if (getter == NoSymbol) addMember(clazz, newGetter(member))
          if (!member.tpe.isInstanceOf[ConstantType] && !member.isLazy) {
            val setter = member.setterIn(clazz)
            if (setter == NoSymbol) addMember(clazz, newSetter(member))
          }
        }
      }
      debuglog("new defs of " + clazz + " = " + clazz.info.decls)
    }
  }

  /** Add all members to be mixed in into a (non-trait-) class
   *  These are:
   *    for every mixin trait T that is not also inherited by the superclass:
   *     add late interface members to T and then:
   *      - if a member M of T is forwarded to the implementation class, add
   *        a forwarder for M unless one exists already.
   *        The alias of the forwarder is the static member it forwards to.
   *      - for every abstract accessor in T, add a field and an implementation for that accessor
   *      - for every super accessor in T, add an implementation of that accessor
   *      - for every module in T, add a module
   */
  def addMixedinMembers(clazz: Symbol, unit: CompilationUnit) {
    def cloneAndAddMixinMember(mixinClass: Symbol, mixinMember: Symbol): Symbol = (
      cloneAndAddMember(mixinClass, mixinMember, clazz)
           setPos clazz.pos
        resetFlag DEFERRED | lateDEFERRED
    )

    /* Mix in members of implementation class mixinClass into class clazz */
    def mixinImplClassMembers(mixinClass: Symbol, mixinInterface: Symbol) {
      if (!mixinClass.isImplClass) devWarning ("Impl class flag is not set " +
        ((mixinClass.debugLocationString, mixinInterface.debugLocationString)))

      for (member <- mixinClass.info.decls ; if isForwarded(member)) {
        val imember = member overriddenSymbol mixinInterface
        imember overridingSymbol clazz match {
          case NoSymbol =>
            if (clazz.info.findMember(member.name, 0, lateDEFERRED, stableOnly = false).alternatives contains imember)
              cloneAndAddMixinMember(mixinInterface, imember).asInstanceOf[TermSymbol] setAlias member
          case _        =>
        }
      }
    }

    /* Mix in members of trait mixinClass into class clazz. Also,
     * for each lazy field in mixinClass, add a link from its mixed in member to its
     * initializer method inside the implclass.
     */
    def mixinTraitMembers(mixinClass: Symbol) {
      // For all members of a trait's interface do:
      for (mixinMember <- mixinClass.info.decls) {
        if (isConcreteAccessor(mixinMember)) {
          if (isOverriddenAccessor(mixinMember, clazz.info.baseClasses))
            devWarning(s"Overridden concrete accessor: ${mixinMember.fullLocationString}")
          else {
            // mixin field accessors
            val mixedInAccessor = cloneAndAddMixinMember(mixinClass, mixinMember)
            if (mixinMember.isLazy) {
              initializer(mixedInAccessor) = (
                implClass(mixinClass).info.decl(mixinMember.name)
                  orElse abort("Could not find initializer for " + mixinMember.name)
              )
            }
            if (!mixinMember.isSetter)
              mixinMember.tpe match {
                case MethodType(Nil, ConstantType(_)) =>
                  // mixinMember is a constant; only getter is needed
                  ;
                case MethodType(Nil, TypeRef(_, UnitClass, _)) =>
                  // mixinMember is a value of type unit. No field needed
                  ;
                case _ => // otherwise mixin a field as well
                  // enteringPhase: the private field is moved to the implementation class by erasure,
                  // so it can no longer be found in the mixinMember's owner (the trait)
                  val accessed = enteringPickler(mixinMember.accessed)
                  // #3857, need to retain info before erasure when cloning (since cloning only
                  // carries over the current entry in the type history)
                  val sym = enteringErasure {
                    // so we have a type history entry before erasure
                    clazz.newValue(mixinMember.localName, mixinMember.pos).setInfo(mixinMember.tpe.resultType)
                  }
                  sym updateInfo mixinMember.tpe.resultType // info at current phase

                  val newFlags = (
                      ( PrivateLocal )
                    | ( mixinMember getFlag MUTABLE | LAZY)
                    | ( if (mixinMember.hasStableFlag) 0 else MUTABLE )
                  )

                  addMember(clazz, sym setFlag newFlags setAnnotations accessed.annotations)
              }
          }
        }
        else if (mixinMember.isSuperAccessor) { // mixin super accessors
          val superAccessor = addMember(clazz, mixinMember.cloneSymbol(clazz)) setPos clazz.pos
          assert(superAccessor.alias != NoSymbol, superAccessor)

          rebindSuper(clazz, mixinMember.alias, mixinClass) match {
            case NoSymbol =>
              reporter.error(clazz.pos, "Member %s of mixin %s is missing a concrete super implementation.".format(
                mixinMember.alias, mixinClass))
            case alias1 =>
              superAccessor.asInstanceOf[TermSymbol] setAlias alias1
          }
        }
        else if (mixinMember.isMethod && mixinMember.isModule && mixinMember.hasNoFlags(LIFTED | BRIDGE)) {
          // mixin objects: todo what happens with abstract objects?
          addMember(clazz, mixinMember.cloneSymbol(clazz, mixinMember.flags & ~(DEFERRED | lateDEFERRED)) setPos clazz.pos)
        }
      }
    }

    if (clazz.isJavaDefined || treatedClassInfos(clazz) == clazz.info)
      return

    treatedClassInfos(clazz) = clazz.info
    assert(!clazz.isTrait && clazz.info.parents.nonEmpty, clazz)

    // first complete the superclass with mixed in members
    addMixedinMembers(clazz.superClass, unit)

    for (mc <- clazz.mixinClasses ; if mc hasFlag lateINTERFACE) {
      // @SEAN: adding trait tracking so we don't have to recompile transitive closures
      unit.depends += mc
      addLateInterfaceMembers(mc)
      mixinTraitMembers(mc)
      mixinImplClassMembers(implClass(mc), mc)
    }
  }

  /** The info transform for this phase does the following:
   *   - The parents of every class are mapped from implementation class to interface
   *   - Implementation classes become modules that inherit nothing
   *     and that define all.
   */
  override def transformInfo(sym: Symbol, tp: Type): Type = tp match {
    case ClassInfoType(parents, decls, clazz) =>
      var parents1 = parents
      var decls1 = decls
      if (!clazz.isPackageClass) {
        exitingMixin(clazz.owner.info)
        if (clazz.isImplClass) {
          clazz setFlag lateMODULE
          var sourceModule = clazz.owner.info.decls.lookup(sym.name.toTermName)
          if (sourceModule == NoSymbol) {
            sourceModule = (
              clazz.owner.newModuleSymbol(sym.name.toTermName, sym.pos, MODULE)
                setModuleClass sym.asInstanceOf[ClassSymbol]
            )
            clazz.owner.info.decls enter sourceModule
          }
          else {
            sourceModule setPos sym.pos
            if (sourceModule.flags != MODULE) {
              log(s"!!! Directly setting sourceModule flags for $sourceModule from ${sourceModule.flagString} to MODULE")
              sourceModule.flags = MODULE
            }
          }
          sourceModule setInfo sym.tpe
          // Companion module isn't visible for anonymous class at this point anyway
          assert(clazz.sourceModule != NoSymbol || clazz.isAnonymousClass,  s"$clazz has no sourceModule: $sym ${sym.tpe}")
          parents1 = List()
          decls1 = newScopeWith(decls.toList filter isImplementedStatically: _*)
        } else if (!parents.isEmpty) {
          parents1 = parents.head :: (parents.tail map toInterface)
        }
      }
      //decls1 = enteringPhase(phase.next)(newScopeWith(decls1.toList: _*))//debug
      if ((parents1 eq parents) && (decls1 eq decls)) tp
      else ClassInfoType(parents1, decls1, clazz)

    case MethodType(params, restp) =>
      toInterfaceMap(
        if (isImplementedStatically(sym)) {
          val ownerParam = sym.newSyntheticValueParam(toInterface(sym.owner.typeOfThis))
          MethodType(ownerParam :: params, restp)
        } else
          tp)

    case _ =>
      tp
  }

// --------- term transformation -----------------------------------------------

  protected def newTransformer(unit: CompilationUnit): Transformer =
    new MixinTransformer(unit)

  class MixinTransformer(unit : CompilationUnit) extends Transformer {
    /** Within a static implementation method: the parameter referring to the
     *  current object.  Undefined everywhere else.
     */
    private var self: Symbol = _

    /** The rootContext used for typing */
    private val rootContext =
      erasure.NoContext.make(EmptyTree, rootMirror.RootClass, newScope)

    /** The typer */
    private var localTyper: erasure.Typer = _
    private def typedPos(pos: Position)(tree: Tree): Tree = localTyper.typedPos(pos)(tree)

    private val lzy = new lazyVals.LazyValues(unit)

    /** The first transform; called in a pre-order traversal at phase mixin
     *  (that is, every node is processed before its children).
     *  What transform does:
     *   - For every non-trait class, add all mixed in members to the class info.
     *   - For every trait, add all late interface members to the class info
     *   - For every static implementation method:
     *       - remove override flag
     *       - create a new method definition that also has a `self` parameter
     *         (which comes first) Iuli: this position is assumed by tail call elimination
     *         on a different receiver. Storing a new 'this' assumes it is located at
     *         index 0 in the local variable table. See 'STORE_THIS' and GenASM.
     *   - Map implementation class types in type-apply's to their interfaces
     *   - Remove all fields in implementation classes
     */
    private def preTransform(tree: Tree): Tree = {
      val sym = tree.symbol
      tree match {
        case Template(parents, self, body) =>
          localTyper = erasure.newTyper(rootContext.make(tree, currentOwner))
          exitingMixin(currentOwner.owner.info)//todo: needed?

          if (!currentOwner.isTrait && !isPrimitiveValueClass(currentOwner))
            addMixedinMembers(currentOwner, unit)
          else if (currentOwner hasFlag lateINTERFACE)
            addLateInterfaceMembers(currentOwner)

          tree
        case DefDef(_, _, _, vparams :: Nil, _, _) =>
          if (currentOwner.isImplClass) {
            if (isImplementedStatically(sym)) {
              sym setFlag notOVERRIDE
              self = sym.newValueParameter(nme.SELF, sym.pos) setInfo toInterface(currentOwner.typeOfThis)
              val selfdef = ValDef(self) setType NoType
              copyDefDef(tree)(vparamss = List(selfdef :: vparams))
            }
            else EmptyTree
          }
          else {
            if (currentOwner.isTrait && sym.isSetter && !enteringPickler(sym.isDeferred)) {
              sym.addAnnotation(TraitSetterAnnotationClass)
            }
            tree
          }
        // !!! What is this doing, and why is it only looking for exactly
        // one type parameter? It would seem to be
        //   "Map implementation class types in type-apply's to their interfaces"
        // from the comment on preTransform, but is there some way we should know
        // that impl class types in type applies can only appear in single
        // type parameter type constructors?
        case Apply(tapp @ TypeApply(fn, List(arg)), List()) =>
          if (arg.tpe.typeSymbol.isImplClass) {
            val ifacetpe = toInterface(arg.tpe)
            arg setType ifacetpe
            tapp setType MethodType(Nil, ifacetpe)
            tree setType ifacetpe
          }
          tree
        case ValDef(_, _, _, _) if currentOwner.isImplClass =>
          EmptyTree
        case _ =>
          tree
      }
    }

    /** Create an identifier which references self parameter.
     */
    private def selfRef(pos: Position) =
      gen.mkAttributedIdent(self) setPos pos

    /** Replace a super reference by this or the self parameter, depending
     *  on whether we are in an implementation class or not.
     *  Leave all other trees unchanged.
     */
    private def transformSuper(tree: Tree) = tree match {
      case Super(qual, _) =>
        transformThis(qual)
      case _ =>
        tree
    }

    /** Replace a this reference to the current implementation class by the self
     *  parameter. Leave all other trees unchanged.
     */
    private def transformThis(tree: Tree) = tree match {
      case This(_) if tree.symbol.isImplClass =>
        assert(tree.symbol == currentOwner.enclClass)
        selfRef(tree.pos)
      case _ =>
        tree
    }

    /** Create a static reference to given symbol `sym` of the
     *  form `M.sym` where M is the symbol's implementation module.
     */
    private def staticRef(sym: Symbol): Tree = {
      sym.owner.info        //todo: needed?
      sym.owner.owner.info  //todo: needed?

      if (sym.owner.sourceModule eq NoSymbol)
        abort(s"Cannot create static reference to $sym because ${sym.safeOwner} has no source module")
      else
        REF(sym.owner.sourceModule) DOT sym
    }

    /** Add all new definitions to a non-trait class
     *  These fall into the following categories:
     *    - for a trait interface:
     *       - abstract accessors for all fields in the implementation class
     *    - for a non-trait class:
     *       - A field for every in a mixin class
     *       - Setters and getters for such fields
     *           - getters for mixed in lazy fields are completed
     *       - module variables and module creators for every module in a mixin class
     *         (except if module is lifted -- in this case the module variable
     *          is local to some function, and the creator method is static.)
     *       - A super accessor for every super accessor in a mixin class
     *       - Forwarders for all methods that are implemented statically
     *  All superaccessors are completed with right-hand sides (@see completeSuperAccessor)
     *  @param clazz  The class to which definitions are added
     */
    private def addNewDefs(clazz: Symbol, stats: List[Tree]): List[Tree] = {
      val newDefs = mutable.ListBuffer[Tree]()

      /* Attribute given tree and anchor at given position */
      def attributedDef(pos: Position, tree: Tree): Tree = {
        debuglog("add new def to " + clazz + ": " + tree)
        typedPos(pos)(tree)
      }

      /* The position of given symbol, or, if this is undefined,
       * the position of the current class.
       */
      def position(sym: Symbol) =
        if (sym.pos == NoPosition) clazz.pos else sym.pos

      /* Add tree at given position as new definition */
      def addDef(pos: Position, tree: Tree) {
        newDefs += attributedDef(pos, tree)
      }

      val addNewDefsImpl = new lzy.AddNewDefs(clazz, stats, localTyper.asInstanceOf[lazyVals.global.analyzer.Typer], addDef _)

      /* Add new method definition.
       *
       * @param sym   The method symbol.
       * @param rhs   The method body.
       */
      def addDefDef(sym: Symbol, rhs: Tree = EmptyTree) = addDef(position(sym), DefDef(sym, rhs))
      def addValDef(sym: Symbol, rhs: Tree = EmptyTree) = addDef(position(sym), ValDef(sym, rhs))

      /* Add `newdefs` to `stats`, removing any abstract method definitions
       * in `stats` that are matched by some symbol defined in
       * `newDefs`.
       */
      def add(stats: List[Tree], newDefs: List[Tree]) = {
        val newSyms = newDefs map (_.symbol)
        def isNotDuplicate(tree: Tree) = tree match {
          case DefDef(_, _, _, _, _, _) =>
            val sym = tree.symbol
            !(sym.isDeferred &&
              (newSyms exists (nsym => nsym.name == sym.name && (nsym.tpe matches sym.tpe))))
          case _ =>
            true
        }
        if (newDefs.isEmpty) stats
        else newDefs ::: (stats filter isNotDuplicate)
      }

      /* If `stat` is a superaccessor, complete it by adding a right-hand side.
       * Note: superaccessors are always abstract until this point.
       *  The method to call in a superaccessor is stored in the accessor symbol's alias field.
       * The rhs is:
       *   super.A(xs)  where A is the super accessor's alias and xs are its formal parameters.
       * This rhs is typed and then mixin transformed.
       */
      def completeSuperAccessor(stat: Tree) = stat match {
        case DefDef(_, _, _, vparams :: Nil, _, EmptyTree) if stat.symbol.isSuperAccessor =>
          val body = atPos(stat.pos)(Apply(Select(Super(clazz, tpnme.EMPTY), stat.symbol.alias), vparams map (v => Ident(v.symbol))))
          val pt   = stat.symbol.tpe.resultType

          copyDefDef(stat)(rhs = enteringMixin(transform(localTyper.typed(body, pt))))
        case _ =>
          stat
      }

      addNewDefsImpl.buildBitmapOffsets()
      var stats1 = addNewDefsImpl.addCheckedGetters(clazz, stats)

      def getterBody(getter: Symbol) = {
        assert(getter.isGetter)
        val readValue = getter.tpe match {
          // A field "final val f = const" in a trait generates a getter with a ConstantType.
          case MethodType(Nil, ConstantType(c)) =>
            Literal(c)
          case _ =>
            // if it is a mixed-in lazy value, complete the accessor
            if (getter.isLazy) {
              val isUnit    = isUnitGetter(getter)
              val initCall  = Apply(staticRef(initializer(getter)), gen.mkAttributedThis(clazz) :: Nil)
              val selection = fieldAccess(getter)
              val init      = if (isUnit) initCall else atPos(getter.pos)(Assign(selection, initCall))
              val returns   = if (isUnit) UNIT else selection
              addNewDefsImpl.mkLazyDef(clazz, getter, List(init), returns, lzy.fieldOffset(getter))
            }
            // For a field of type Unit in a trait, no actual field is generated when being mixed in.
            else if (isUnitGetter(getter)) UNIT
            else fieldAccess(getter)
        }
        if (!lazyVals.needsInitFlag(getter)) readValue
        else addNewDefsImpl.mkCheckedAccessor(clazz, readValue, lzy.fieldOffset(getter), getter.pos, getter)
      }

      def setterBody(setter: Symbol) = {
        val getter = setter.getterIn(clazz)

        // A trait with a field of type Unit creates a trait setter (invoked by the
        // implementation class constructor), like for any other trait field.
        // However, no actual field is created in the class that mixes in the trait.
        // Therefore the setter does nothing (except setting the -Xcheckinit flag).

        val setInitFlag =
          if (!lazyVals.needsInitFlag(getter)) Nil
          else List(addNewDefsImpl.mkSetFlag(clazz, lzy.fieldOffset(getter), getter, lzy.bitmapKind(getter)))

        val fieldInitializer =
          if (isUnitGetter(getter)) Nil
          else List(Assign(fieldAccess(setter), Ident(setter.firstParam)))

        (fieldInitializer ::: setInitFlag) match {
          case Nil => UNIT
          // If there's only one statement, the Block factory does not actually create a Block.
          case stats => Block(stats: _*)
        }
      }

      def isUnitGetter(getter: Symbol) = getter.tpe.resultType.typeSymbol == UnitClass
      def fieldAccess(accessor: Symbol) = Select(This(clazz), accessor.accessed)

      def isOverriddenSetter(sym: Symbol) =
        nme.isTraitSetterName(sym.name) && {
          val other = sym.nextOverriddenSymbol
          isOverriddenAccessor(other.getterIn(other.owner), clazz.info.baseClasses)
        }

      // for all symbols `sym` in the class definition, which are mixed in:
      for (sym <- clazz.info.decls ; if sym hasFlag MIXEDIN) {
        // if current class is a trait interface, add an abstract method for accessor `sym`
        if (clazz hasFlag lateINTERFACE) {
          addDefDef(sym)
        }
        // if class is not a trait add accessor definitions
        else if (!clazz.isTrait) {
          if (isConcreteAccessor(sym)) {
            // add accessor definitions
            addDefDef(sym, {
              if (sym.isSetter) {
                // If this is a setter of a mixed-in field which is overridden by another mixin,
                // the trait setter of the overridden one does not need to do anything - the
                // trait setter of the overriding field will initialize the field.
                if (isOverriddenSetter(sym)) UNIT
                else setterBody(sym)
              }
              else getterBody(sym)
            })
          }
          else if (sym.isModule && !(sym hasFlag LIFTED | BRIDGE)) {
            // Moved to Refchecks
          }
          else if (!sym.isMethod) {
            // add fields
            addValDef(sym)
          }
          else if (sym.isSuperAccessor) {
            // add superaccessors
            addDefDef(sym)
          }
          else {
            // add forwarders
            assert(sym.alias != NoSymbol, sym)
            // debuglog("New forwarder: " + sym.defString + " => " + sym.alias.defString)
            if (!sym.isMacro) addDefDef(sym, Apply(staticRef(sym.alias), gen.mkAttributedThis(clazz) :: sym.paramss.head.map(Ident)))
          }
        }
      }
      stats1 = add(stats1, newDefs.toList)
      if (!clazz.isTrait) stats1 = stats1 map completeSuperAccessor
      stats1
    }

    private def nullableFields(templ: Template): Map[Symbol, Set[Symbol]] = {
      val scope = templ.symbol.owner.info.decls
      // if there are no lazy fields, take the fast path and save a traversal of the whole AST
      if (scope exists (_.isLazy)) {
        val map = mutable.Map[Symbol, Set[Symbol]]() withDefaultValue Set()
        // check what fields can be nulled for
        for ((field, users) <- lazyVals.singleUseFields(templ); lazyFld <- users if !lazyFld.accessed.hasAnnotation(TransientAttr))
          map(lazyFld) += field

        map.toMap
      }
      else Map()
    }

    /** The transform that gets applied to a tree after it has been completely
     *  traversed and possible modified by a preTransform.
     *  This step will
     *    - change every node type that refers to an implementation class to its
     *      corresponding interface, unless the node's symbol is an implementation class.
     *    - change parents of templates to conform to parents in the symbol info
     *    - add all new definitions to a class or interface
     *    - remove widening casts
     *    - change calls to methods which are defined only in implementation classes
     *      to static calls of methods in implementation modules (@see staticCall)
     *    - change super calls to methods in implementation classes to static calls
     *      (@see staticCall)
     *    - change `this` in implementation modules to references to the self parameter
     *    - refer to fields in some implementation class via an abstract method in the interface.
     */
    private def postTransform(tree: Tree): Tree = {
      def siteWithinImplClass = currentOwner.enclClass.isImplClass
      val sym = tree.symbol

      // change every node type that refers to an implementation class to its
      // corresponding interface, unless the node's symbol is an implementation class.
      if (tree.tpe.typeSymbol.isImplClass && ((sym eq null) || !sym.isImplClass))
        tree modifyType toInterface

      tree match {
        case templ @ Template(parents, self, body) =>
          // change parents of templates to conform to parents in the symbol info
          val parents1 = currentOwner.info.parents map (t => TypeTree(t) setPos tree.pos)
          // mark fields which can be nulled afterward
          lzy.lazyValNullables = nullableFields(templ) withDefaultValue Set()
          // add all new definitions to current class or interface
          treeCopy.Template(tree, parents1, self, addNewDefs(currentOwner, body))

        // remove widening casts
        case Apply(TypeApply(Select(qual, _), targ :: _), _) if isCastSymbol(sym) && (qual.tpe <:< targ.tpe) =>
          qual

        case Apply(Select(qual, _), args) =>
          /*  Changes `qual.m(args)` where m refers to an implementation
           *  class method to Q.m(S, args) where Q is the implementation module of
           *  `m` and S is the self parameter for the call, which
           *  is determined as follows:
           *     - if qual != super, qual itself
           *     - if qual == super, and we are in an implementation class,
           *       the current self parameter.
           *     - if qual == super, and we are not in an implementation class, `this`
           */
          def staticCall(target: Symbol) = {
            def implSym = implClass(sym.owner).info.member(sym.name)
            assert(target ne NoSymbol,
              List(sym + ":", sym.tpe, sym.owner, implClass(sym.owner), implSym,
                  enteringPrevPhase(implSym.tpe), phase) mkString " "
            )
            typedPos(tree.pos)(Apply(staticRef(target), transformSuper(qual) :: args))
          }

          if (isStaticOnly(sym)) {
            // change calls to methods which are defined only in implementation
            // classes to static calls of methods in implementation modules
            staticCall(sym)
          }
          else qual match {
            case Super(_, mix) =>
              // change super calls to methods in implementation classes to static calls.
              // Transform references super.m(args) as follows:
              //  - if `m` refers to a trait, insert a static call to the corresponding static
              //    implementation
              //  - otherwise return tree unchanged
              assert(
                !(mix == tpnme.EMPTY && siteWithinImplClass),
                "illegal super in trait: " + currentOwner.enclClass + " " + tree
              )

              if (sym.owner hasFlag lateINTERFACE) {
                if (sym.hasAccessorFlag) {
                  assert(args.isEmpty, args)
                  val sym1 = sym.overridingSymbol(currentOwner.enclClass)
                  typedPos(tree.pos)((transformSuper(qual) DOT sym1)())
                }
                else {
                  staticCall(enteringPrevPhase(sym.overridingSymbol(implClass(sym.owner))))
                }
              }
              else {
                assert(!siteWithinImplClass, currentOwner.enclClass)
                tree
              }
            case _ =>
              tree
          }

        case This(_) =>
          transformThis(tree)

        case Select(Super(_, _), name) =>
          tree

        case Select(qual, name) if sym.owner.isImplClass && !isStaticOnly(sym) =>
          assert(!sym.isMethod, "no method allowed here: %s%s %s".format(sym, sym.isImplOnly, sym.flagString))
          // refer to fields in some implementation class via an abstract
          // getter in the interface.
          val iface  = toInterface(sym.owner.tpe).typeSymbol
          val ifaceGetter = sym getterIn iface

          if (ifaceGetter == NoSymbol) abort("No getter for " + sym + " in " + iface)
          else typedPos(tree.pos)((qual DOT ifaceGetter)())

        case Assign(Apply(lhs @ Select(qual, _), List()), rhs) =>
          // assign to fields in some implementation class via an abstract
          // setter in the interface.
          def setter = lhs.symbol.setterIn(toInterface(lhs.symbol.owner.tpe).typeSymbol) setPos lhs.pos

          typedPos(tree.pos)((qual DOT setter)(rhs))

        case _ =>
          tree
      }
    }

    /** The main transform method.
     *  This performs pre-order traversal preTransform at mixin phase;
     *  when coming back, it performs a postTransform at phase after.
     */
    override def transform(tree: Tree): Tree = {
      val saved = localTyper
      val tree1 = super.transform(preTransform(tree))
      // localTyper needed when not flattening inner classes. parts after an
      // inner class will otherwise be typechecked with a wrong scope
      try exitingMixin(postTransform(tree1))
      finally localTyper = saved
    }
  }
}
