/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc
package transform

import scala.annotation._
import scala.reflect.internal.ClassfileConstants._
import scala.collection.{immutable, mutable}
import symtab._
import Flags._
import scala.reflect.internal.Mode._
import scala.reflect.internal.util.FreshNameCreator

abstract class Erasure extends InfoTransform
                          with scala.reflect.internal.transform.Erasure
                          with TypingTransformers
                          with ast.TreeDSL
                          with TypeAdaptingTransformer
{
  import global._
  import definitions._
  import CODE._

  val phaseName: String = "erasure"

  val requiredDirectInterfaces = perRunCaches.newMap[Symbol, mutable.Set[Symbol]]()

  def newTransformer(unit: CompilationUnit): AstTransformer =
    new ErasureTransformer(unit)

  override def keepsTypeParams = false

// -------- erasure on types --------------------------------------------------------

  // convert a numeric with a toNNN method
  def numericConversion(tree: Tree, numericSym: Symbol): Tree = {
    val mname      = newTermName("to" + numericSym.name)
    val conversion = tree.tpe member mname

    assert(conversion != NoSymbol, s"$tree => $numericSym")
    atPos(tree.pos)(Apply(Select(tree, conversion), Nil))
  }

  private object NeedsSigCollector {
    private val NeedsSigCollector_true = new NeedsSigCollector(isClassConstructor = true)
    private val NeedsSigCollector_false = new NeedsSigCollector(isClassConstructor = false)
    def apply(isClassConstructor: Boolean) = if (isClassConstructor) NeedsSigCollector_true else NeedsSigCollector_false
  }
  private class NeedsSigCollector(isClassConstructor: Boolean) extends TypeCollector(initial = false) {
    def apply(tp: Type): Unit =
      if (!result) {
        tp match {
          case st: SubType =>
            apply(st.supertype)
          case TypeRef(pre, sym, args) =>
            if (sym == ArrayClass) untilApply(args)
            else if (sym.isTypeParameterOrSkolem || sym.isExistentiallyBound || !args.isEmpty) result = true
            else if (sym.isClass) apply(rebindInnerClass(pre, sym)) // #2585
            else if (!sym.isTopLevel) apply(pre)
          case PolyType(_, _) | ExistentialType(_, _) => result = true
          case RefinedType(parents, _) =>
            untilApply(parents)
          case ClassInfoType(parents, _, _) =>
            untilApply(parents)
          case AnnotatedType(_, atp) =>
            apply(atp)
          case MethodType(params, resultType) =>
            if (isClassConstructor) {
              val sigParams = params match {
                case head :: tail if head.isOuterParam => tail
                case _ => params
              }
              this.foldOver(sigParams)
              // skip the result type, it is Void in the signature.
            } else {
              tp.foldOver(this)
            }
          case _ =>
            tp.foldOver(this)
        }
      }
    @tailrec
    private def untilApply(ts: List[Type]): Unit =
      ts match {
        case t :: ts if !result =>
          apply(t)
          untilApply(ts)
        case _ =>
      }
  }

  override protected def verifyJavaErasure = settings.Xverify.value || settings.isDebug
  def needsJavaSig(sym: Symbol, tp: Type, throwsArgs: List[Type]) = !settings.Ynogenericsig.value && {
    def needs(tp: Type) = NeedsSigCollector(sym.isClassConstructor).collect(tp)
    needs(tp) || throwsArgs.exists(needs)
  }

  // only refer to type params that will actually make it into the sig, this excludes:
  // * higher-order type parameters
  // * type parameters appearing in method parameters
  // * type members not visible in an enclosing template
  private def isTypeParameterInSig(sym: Symbol, initialSymbol: Symbol) = (
    !sym.isHigherOrderTypeParameter &&
    sym.isTypeParameterOrSkolem && (
      (initialSymbol.isMethod && initialSymbol.typeParams.contains(sym)) ||
      (initialSymbol.ownersIterator.exists(encl => encl.isClass && !encl.hasPackageFlag && sym.isNestedIn(encl)))
    )
  )

  /** This object is only used for confidence checking when -check:genjvm is set.
   *  In that case we make sure that the erasure of the `normalized` type
   *  is the same as the erased type that's generated. Normalization means
   *  unboxing some primitive types and further simplifications as they are done in jsig.
   */
  val prepareSigMap: TypeMap = new TypeMap {
    def squashBoxed(tp: Type): Type = tp.dealiasWiden match {
      case RefinedType(parents, decls) =>
        val parents1 = parents mapConserve squashBoxed
        if (parents1 eq parents) tp
        else RefinedType(parents1, decls)
      case t @ ExistentialType(tparams, tpe) =>
        val tpe1 = squashBoxed(tpe)
        if (tpe1 eq tpe) t
        else ExistentialType(tparams, tpe1)
      case t =>
        if (boxedClass contains t.typeSymbol) ObjectTpe
        else tp
    }
    def apply(tp: Type): Type = tp.dealiasWiden match {
      case tp1 @ TypeBounds(lo, hi) =>
        val lo1 = squashBoxed(apply(lo))
        val hi1 = squashBoxed(apply(hi))
        if ((lo1 eq lo) && (hi1 eq hi)) tp1
        else TypeBounds(lo1, hi1)
      case tp1 @ TypeRef(pre, sym, args) =>
        def argApply(tp: Type) = {
          val tp1 = apply(tp)
          if (tp1.typeSymbol == UnitClass) ObjectTpe
          else squashBoxed(tp1)
        }
        if (sym == ArrayClass && args.nonEmpty)
          if (unboundedGenericArrayLevel(tp1) == 1) ObjectTpe
          else mapOver(tp1)
        else if (sym == AnyClass || sym == AnyValClass || sym == SingletonClass)
          ObjectTpe
        else if (sym == UnitClass)
          BoxedUnitTpe
        else if (sym == NothingClass)
          RuntimeNothingClass.tpe
        else if (sym == NullClass)
          RuntimeNullClass.tpe
        else {
          val pre1 = apply(pre)
          val args1 = args mapConserve argApply
          if ((pre1 eq pre) && (args1 eq args)) tp1
          else TypeRef(pre1, sym, args1)
        }
      case tp1 @ MethodType(params, restpe) =>
        val params1 = mapOver(params)
        val restpe1 = if (restpe.typeSymbol == UnitClass) UnitTpe else apply(restpe)
        if ((params1 eq params) && (restpe1 eq restpe)) tp1
        else MethodType(params1, restpe1)
      case tp1 @ RefinedType(parents, decls) =>
        val parents1 = parents mapConserve apply
        if (parents1 eq parents) tp1
        else RefinedType(parents1, decls)
      case t @ ExistentialType(tparams, tpe) =>
        val tpe1 = apply(tpe)
        if (tpe1 eq tpe) t
        else ExistentialType(tparams, tpe1)
      case tp1: ClassInfoType =>
        tp1
      case tp1 =>
        mapOver(tp1)
    }
  }

  private def hiBounds(bounds: TypeBounds): List[Type] = bounds.hi.dealiasWiden match {
    case RefinedType(parents, _) => parents map (_.dealiasWiden)
    case tp                      => tp :: Nil
  }

  private def isErasedValueType(tpe: Type) = tpe.isInstanceOf[ErasedValueType]

  /* Drop redundant types (ones which are implemented by some other parent) from the immediate parents.
   * This is important on Android because there is otherwise an interface explosion.
   */
  def minimizeParents(cls: Symbol, parents: List[Type]): List[Type] = if (parents.isEmpty) parents else {
    val requiredDirect: Symbol => Boolean = requiredDirectInterfaces.getOrElse(cls, Set.empty)
    var rest   = parents.tail
    var leaves = collection.mutable.ListBuffer.empty[Type] += parents.head
    while (rest.nonEmpty) {
      val candidate = rest.head
      val candidateSym = candidate.typeSymbol
      val required = requiredDirect(candidateSym) || !leaves.exists(t => t.typeSymbol isSubClass candidateSym)
      if (required) {
        leaves = leaves filter { t =>
          val ts = t.typeSymbol
          requiredDirect(ts) || !ts.isTraitOrInterface || !candidateSym.isSubClass(ts)
        }
        leaves += candidate
      }
      rest = rest.tail
    }
    leaves.toList
  }


  /** The Java signature of type 'info', for symbol sym. The symbol is used to give the right return
   *  type for constructors.
   */

  final def javaSig(sym0: Symbol, info: Type, markClassUsed: Symbol => Unit): Option[String] = enteringErasure { javaSig0(sym0, info, markClassUsed) }
  @noinline
  private final def javaSig0(sym0: Symbol, info: Type, markClassUsed: Symbol => Unit): Option[String] = {
    val builder = new java.lang.StringBuilder(64)
    val isTraitSignature = sym0.enclClass.isTrait

    def superSig(cls: Symbol, parents: List[Type]): Unit = {
      def isInterfaceOrTrait(sym: Symbol) = sym.isInterface || sym.isTrait

      // a signature should always start with a class
      def ensureClassAsFirstParent(tps: List[Type]) = tps match {
        case Nil => ObjectTpe :: Nil
        case head :: _ if isInterfaceOrTrait(head.typeSymbol) => ObjectTpe :: tps
        case _ => tps
      }

      val minParents = minimizeParents(cls, parents)
      val validParents =
        if (isTraitSignature)
          // java is unthrilled about seeing interfaces inherit from classes
          minParents filter (p => isInterfaceOrTrait(p.typeSymbol))
        else minParents

      val ps = ensureClassAsFirstParent(validParents)
      ps.foreach(boxedSig)
    }
    def boxedSig(tp: Type): Unit = jsig(tp, unboxedVCs = false)
    def boundsSig(bounds: List[Type]): Unit = {
      val (isTrait, isClass) = partitionConserve(bounds)(_.typeSymbol.isTrait)
      isClass match {
        case Nil    => builder.append(':') // + boxedSig(ObjectTpe)
        case x :: _ => builder.append(':'); boxedSig(x)
      }
      isTrait.foreach { tp =>
        builder.append(':')
        boxedSig(tp)
      }
    }
    def paramSig(tsym: Symbol): Unit = {
      builder.append(tsym.name)
      boundsSig(hiBounds(tsym.info.bounds))
    }
    def polyParamSig(tparams: List[Symbol]): Unit = (
      if (!tparams.isEmpty) {
        builder.append('<')
        tparams foreach paramSig
        builder.append('>')
      }
    )

    // Anything which could conceivably be a module (i.e. isn't known to be
    // a type parameter or similar) must go through here or the signature is
    // likely to end up with Foo<T>.Empty where it needs Foo<T>.Empty$.
    def fullNameInSig(sym: Symbol): Unit = builder.append('L').append(enteringJVM(sym.javaBinaryNameString))

    @noinline
    def jsig(tp0: Type, existentiallyBound: List[Symbol] = Nil, toplevel: Boolean = false, unboxedVCs: Boolean = true): Unit = {
      @inline def jsig1(tp0: Type) = jsig(tp0, existentiallyBound = Nil, toplevel = false, unboxedVCs = true)
      val tp = tp0.dealias
      tp match {
        case st: SubType =>
          jsig(st.supertype, existentiallyBound, toplevel, unboxedVCs)
        case ExistentialType(tparams, tpe) =>
          jsig(tpe, tparams, toplevel, unboxedVCs)
        case TypeRef(pre, sym, args) =>
          def argSig(tp: Type): Unit =
            if (existentiallyBound contains tp.typeSymbol) {
              val bounds = tp.typeSymbol.info.bounds
              if (!(AnyRefTpe <:< bounds.hi)) {
                builder.append('+')
                boxedSig(bounds.hi)
              }
              else if (!(bounds.lo <:< NullTpe)) {
                builder.append('-')
                boxedSig(bounds.lo)
              }
              else builder.append('*')
            } else tp match {
              case PolyType(_, _) =>
                builder.append('*') // scala/bug#7932
              case _ =>
                boxedSig(tp)
            }
          def classSig(): Unit = {
            markClassUsed(sym)
            val preRebound = pre.baseType(sym.owner) // #2585
            if (needsJavaSig(sym, preRebound, Nil)) {
              val i = builder.length()
              jsig(preRebound, existentiallyBound, toplevel = false, unboxedVCs = true)
              if (builder.charAt(i) == 'L') {
                builder.delete(builder.length() - 1, builder.length())// delete ';'
                // If the prefix is a module, drop the '$'. Classes (or modules) nested in modules
                // are separated by a single '$' in the filename: `object o { object i }` is o$i$.
                if (preRebound.typeSymbol.isModuleClass)
                  builder.delete(builder.length() - 1, builder.length())

                // Ensure every '.' in the generated signature immediately follows
                // a close angle bracket '>'.  Any which do not are replaced with '$'.
                // This arises due to multiply nested classes in the face of the
                // rewriting explained at rebindInnerClass.

                // TODO revisit this. Does it align with javac for code that can be expressed in both languages?
                val delimiter = if (builder.charAt(builder.length() - 1) == '>') '.' else '$'
                builder.append(delimiter).append(sym.javaSimpleName)
              } else fullNameInSig(sym)
            } else fullNameInSig(sym)

            if (!args.isEmpty) {
              builder.append('<')
              args foreach argSig
              builder.append('>')
            }
            builder.append(';')
          }

          // If args isEmpty, Array is being used as a type constructor
          if (sym == ArrayClass && args.nonEmpty) {
            if (unboundedGenericArrayLevel(tp) == 1) jsig1(ObjectTpe)
            else {
              builder.append(ARRAY_TAG)
              args.foreach(jsig1(_))
            }
          }
          else if (isTypeParameterInSig(sym, sym0)) {
            assert(!sym.isAliasType, "Unexpected alias type: " + sym)
            builder.append(TVAR_TAG).append(sym.name).append(';')
          }
          else if (sym == AnyClass || sym == AnyValClass || sym == SingletonClass)
            jsig1(ObjectTpe)
          else if (sym == UnitClass)
            jsig1(BoxedUnitTpe)
          else if (sym == NothingClass)
            jsig1(RuntimeNothingClass.tpe)
          else if (sym == NullClass)
            jsig1(RuntimeNullClass.tpe)
          else if (isPrimitiveValueClass(sym)) {
            if (!unboxedVCs) jsig1(ObjectTpe)
            else if (sym == UnitClass) jsig1(BoxedUnitTpe)
            else builder.append(abbrvTag(sym))
          }
          else if (sym.isDerivedValueClass) {
            if (unboxedVCs) {
              val unboxedSeen = (tp memberType sym.derivedValueClassUnbox).finalResultType
              jsig(unboxedSeen, existentiallyBound, toplevel, unboxedVCs = true)
            } else classSig()
          }
          else if (sym.isClass)
            classSig()
          else
            jsig(erasure(sym0)(tp), existentiallyBound, toplevel, unboxedVCs)
        case PolyType(tparams, restpe) =>
          assert(tparams.nonEmpty, tparams)
          if (toplevel) polyParamSig(tparams)
          jsig1(restpe)

        case MethodType(params, restpe) =>
          builder.append('(')
          params foreach (p => {
            val isClassOuterParam = sym0.isClassConstructor && p.isOuterParam
            if (!isClassOuterParam) {
              val tp = p.attachments.get[TypeParamVarargsAttachment] match {
                case Some(att) =>
                  // For @varargs forwarders, a T* parameter has type Array[Object] in the forwarder
                  // instead of Array[T], as the latter would erase to Object (instead of Array[Object]).
                  // To make the generic signature correct ("[T", not "[Object"), an attachment on the
                  // parameter symbol stores the type T that was replaced by Object.
                  builder.append('['); att.typeParamRef
                case _ => p.tpe
              }
              jsig1(tp)
            }
          })
          builder.append(')')
          if (restpe.typeSymbol == UnitClass || sym0.isConstructor) builder.append(VOID_TAG) else jsig1(restpe)

        case RefinedType(parents, decls) =>
          jsig(intersectionDominator(parents), existentiallyBound = Nil, toplevel = false, unboxedVCs = unboxedVCs)
        case ClassInfoType(parents, _, _) =>
          superSig(tp.typeSymbol, parents)
        case AnnotatedType(_, atp) =>
          jsig(atp, existentiallyBound, toplevel, unboxedVCs)
        case BoundedWildcardType(bounds) =>
          println("something's wrong: "+sym0+":"+sym0.tpe+" has a bounded wildcard type")
          jsig(bounds.hi, existentiallyBound, toplevel, unboxedVCs)
        case _ =>
          val etp = erasure(sym0)(tp)
          if (etp == tp) throw new UnknownSig
          else jsig1(etp)
      }
    }
    val throwsArgs = sym0.annotations flatMap ThrownException.unapply
    if (needsJavaSig(sym0, info, throwsArgs)) {
      try {
        jsig(info, toplevel = true)
        throwsArgs.foreach { t =>
          builder.append('^')
          jsig(t, toplevel = true)
        }
        Some(builder.toString)
      }
      catch { case _: UnknownSig => None }
    }
    else None
  }

  class UnknownSig extends Exception

  /** Add calls to supermixin constructors
    *    `super[mix].$init$()`
    *  to tree, which is assumed to be the body of a constructor of class clazz.
    */
  private def addMixinConstructorCalls(tree: Tree, clazz: Symbol): Tree = {
    // TODO: move to constructors?
    def mixinConstructorCalls: List[Tree] = {
      for (mc <- clazz.mixinClasses.reverse if mc.isTrait && mc.primaryConstructor != NoSymbol)
        yield atPos(tree.pos) {
          Apply(SuperSelect(clazz, mc.primaryConstructor), Nil)
        }
    }

    tree match {
      case Block(Nil, expr) =>
        // AnyVal constructor - have to provide a real body so the
        // jvm doesn't throw a VerifyError. But we can't add the
        // body until now, because the typer knows that Any has no
        // constructor and won't accept a call to super.init.
        assert((clazz isSubClass AnyValClass) || clazz.info.parents.isEmpty, clazz)
        Block(List(Apply(gen.mkSuperInitCall, Nil)), expr)

      case Block(stats, expr) =>
        // needs `hasSymbolField` check because `supercall` could be a block (named / default args)
        val (presuper, supercall :: rest) = stats span (t => t.hasSymbolWhich(_ hasFlag PRESUPER)): @unchecked
        treeCopy.Block(tree, presuper ::: (supercall :: mixinConstructorCalls ::: rest), expr)

      case x => throw new MatchError(x)
    }
  }


  val deconstMap = new TypeMap {
    // For some reason classOf[Foo] creates ConstantType(Constant(tpe)) with an actual Type for tpe,
    // which is later translated to a Class. Unfortunately that means we have bugs like the erasure
    // of Class[Foo] and classOf[Bar] not being seen as equivalent, leading to duplicate method
    // generation and failing bytecode. See ticket #4753.
    def apply(tp: Type): Type = tp match {
      case PolyType(_, _)                  => mapOver(tp)
      case MethodType(_, _)                => mapOver(tp)     // nullarymethod was eliminated during uncurry
      case ConstantType(Constant(_: Type)) => ClassClass.tpe  // all classOfs erase to Class
      case ConstantType(value)             => value.tpe.deconst
      case _                               => tp.deconst
    }
  }

  // ## requires a little translation
  private lazy val poundPoundMethods = Set[Symbol](Any_##, Object_##)
  // Methods on Any/Object which we rewrite here while we still know what
  // is a primitive and what arrived boxed.
  private lazy val interceptedMethods = poundPoundMethods ++ primitiveGetClassMethods

// -------- erasure on trees ------------------------------------------

  /** Re-typechecks trees after erasure, using `Eraser` as its typer. */
  object erasureAnalyzer extends {
    val global: Erasure.this.global.type = Erasure.this.global
  } with typechecker.AbstractAnalyzer {
    override def newTyper(context: Context): Typer = new Eraser(context)
  }
  import erasureAnalyzer.{Context, OriginalTreeAttachment, Typer, newTyper, rootContextPostTyper}

  class EnterBridges(@unused unit: CompilationUnit, root: Symbol) {
    val site         = root.thisType
    val bridgesScope = newScope
    val bridgeTarget = mutable.HashMap[Symbol, Symbol]()

    val opc = enteringExplicitOuter { new overridingPairs.BridgesCursor(root) }

    def computeAndEnter(): Unit = {
      while (opc.hasNext) {
        if (enteringExplicitOuter(!opc.low.isDeferred))
          checkPair(opc.currentPair)

        opc.next()
      }
    }

    /** Check that a bridge only overrides members that are also overridden by the original member.
     *
     *  That is, check that the signature of the bridge method does not accidentally override some
     *  other method, possibly written by the user or inherited.
     *
     *  As an optimization, only perform this check for susceptible bridges.
     *
     *  This test is necessary for members that have a value class in their type.
     *  Such members are special because their types after erasure and after post-erasure differ.
     *  This means we generate them after erasure, but the post-erasure transform might introduce
     *  a name clash. The present method guards against these name clashes.
     *
     *  A bridge might also introduce a signature that accidentally matches an existing method.
     *  In that case, it will have a parameter erased to the upper bound of a type parameter
     *  in the signature of the member overridden by the original member. (That upper bound might
     *  be `Object`.) By contrast, erasure of a parameter `List[A]` would already have induced an error
     *  if there were a matching member.
     *
     *  @param  member   The original member
     *  @param  other    The overridden symbol for which the bridge was generated
     *  @param  bridge   The bridge
     */
    def checkBridgeOverrides(member: Symbol, other: Symbol, bridge: Symbol): scala.collection.Seq[(Position, String)] = {
      def fulldef(sym: Symbol) =
        if (sym == NoSymbol) sym.toString
        else s"$sym: ${sym.tpe} in ${sym.owner}"
      val clashErrors = mutable.Buffer[(Position, String)]()
      def clashError(what: String) = {
        val pos = if (member.owner == root) member.pos else root.pos
        val msg = sm"""bridge generated for member ${fulldef(member)}
                      |which overrides ${fulldef(other)}
                      |clashes with definition of $what;
                      |both have erased type ${exitingPostErasure(bridge.tpe)}"""
        clashErrors += Tuple2(pos, msg)
      }
      for (bc <- root.baseClasses) {
        if (settings.isDebug)
          exitingPostErasure(debuglog(
            sm"""check bridge overrides in $bc
                |${bc.info.nonPrivateDecl(bridge.name)}
                |${site.memberType(bridge)}
                |${site.memberType(bc.info.nonPrivateDecl(bridge.name) orElse IntClass)}
                |${(bridge.matchingSymbol(bc, site))}"""))

        def overriddenBy(sym: Symbol) =
          sym.matchingSymbol(bc, site).alternatives filter (sym => !sym.isBridge)
        for (overBridge <- exitingPostErasure(overriddenBy(bridge))) {
          if (overBridge == member) {
            clashError("the member itself")
          } else {
            val overMembers = overriddenBy(member)
            if (!overMembers.exists(overMember =>
              exitingPostErasure(overMember.tpe =:= overBridge.tpe))) {
              clashError(fulldef(overBridge))
            }
          }
        }
      }
      clashErrors
    }

    /** TODO - work through this logic with a fine-toothed comb, incorporating
     *  into SymbolPairs where appropriate.
     */
    def checkPair(pair: SymbolPair): Unit = {
      import pair._
      val member = low
      val other  = high
      val otpe   = highErased

      val bridgeNeeded = exitingErasure {
        def hasBridge = {
          var e = bridgesScope.lookupEntry(member.name)
          while ((e ne null) && !(e.sym.tpe =:= otpe && bridgeTarget(e.sym) == member))
            e = bridgesScope.lookupNextEntry(e)
          e ne null
        }
        !member.isMacro &&
        !(other.tpe =:= member.tpe) &&
        !(deconstMap(other.tpe) =:= deconstMap(member.tpe)) &&
        !hasBridge
      }
      def addBridgeIfOK(): Unit = {
        var newFlags = (member.flags | BRIDGE | ARTIFACT) & ~(ACCESSOR | DEFERRED | LAZY | FINAL)
        // If `member` is a ModuleSymbol, the bridge should not also be a ModuleSymbol. Otherwise we
        // end up with two module symbols with the same name in the same scope, which is surprising
        // when implementing later phases.
        if (member.isModule) newFlags = (newFlags | METHOD) & ~(MODULE | STABLE)
        val bridge = other.cloneSymbolImpl(root, newFlags).setPos(root.pos).setAnnotations(member.annotations)

        debuglog(s"generating bridge from $other (${flagsToString(newFlags)}): ${otpe}${other.locationString} to $member: ${specialErasure(root)(member.tpe)}${member.locationString}")

        // the parameter symbols need to have the new owner
        bridge setInfo (otpe cloneInfo bridge)
        bridgeTarget(bridge) = member

        val shouldAdd = {
          val sigContainsValueClass = member.tpe.exists(_.typeSymbol.isDerivedValueClass)
          def bridgeMayClash = other.paramss.exists(_.exists(_.tpe match { case TypeRef(_, r, _) => r.isTypeParameter case _ => false }))
          def bridgeIsAOK = checkBridgeOverrides(member, other, bridge) match {
            case Nil => true
            case _ if member.owner.isAnonymousClass => resolveAnonymousBridgeClash(member, bridge); true
            case es => for ((pos, msg) <- es) reporter.error(pos, msg); false
          }
          !sigContainsValueClass && !bridgeMayClash || bridgeIsAOK
        }
        if (shouldAdd) {
          exitingErasure(root.info.decls enter bridge)

          bridgesScope enter bridge
          addBridge(bridge, member, other) // GenerateBridges.addBridge bridges ::= makeBridgeDefDef(bridge, member, other)
        }
      }
      if (bridgeNeeded) addBridgeIfOK()
    }

    protected def addBridge(bridge: Symbol, member: Symbol, other: Symbol): Unit = {} // hook for GenerateBridges
  }

  class GenerateBridges(unit: CompilationUnit, root: Symbol) extends EnterBridges(unit, root) {

    var bridges     = List[Tree]()
    var toBeRemoved = immutable.Set.empty[Symbol]

    def generate(): (List[Tree], immutable.Set[Symbol]) = {
      super.computeAndEnter()
      (bridges, toBeRemoved)
    }

    override def addBridge(bridge: Symbol, member: Symbol, other: Symbol): Unit = {
      if (other.owner == root) {
        exitingErasure(root.info.decls.unlink(other))
        toBeRemoved += other
      }
      bridges ::= makeBridgeDefDef(bridge, member, other)
    }

    final def makeBridgeDefDef(bridge: Symbol, member: Symbol, other: Symbol) = exitingErasure {
      // type checking ensures we can safely call `other`, but unless `member.tpe <:< other.tpe`,
      // calling `member` is not guaranteed to succeed in general, there's
      // nothing we can do about this, except for an unapply: when this subtype test fails,
      // return None without calling `member`
      //
      // TODO: should we do this for user-defined unapplies as well?
      // does the first argument list have exactly one argument -- for user-defined unapplies we can't be sure
      def maybeWrap(bridgingCall: Tree): Tree = {
        val guardExtractor = ( // can't statically know which member is going to be selected, so don't let this depend on member.isSynthetic
             (member.name == nme.unapply || member.name == nme.unapplySeq)
          && !exitingErasure((member.tpe <:< other.tpe))) // no static guarantees (TODO: is the subtype test ever true?)

        import CODE._
        val _false    = FALSE
        val pt        = member.tpe.resultType
        lazy val zero =
          if      (_false.tpe <:< pt)    _false
          else if (NoneModule.tpe <:< pt) REF(NoneModule)
          else EmptyTree

        if (guardExtractor && (zero ne EmptyTree)) {
          val typeTest = gen.mkIsInstanceOf(REF(bridge.firstParam), member.tpe.params.head.tpe)
          IF (typeTest) THEN bridgingCall ELSE zero
        } else bridgingCall
      }
      val rhs = member.tpe match {
        case MethodType(Nil, FoldableConstantType(c)) => Literal(c)
        case _                                =>
          val sel: Tree    = gen.mkAttributedSelect(gen.mkAttributedThis(root), member)
          val bridgingCall = bridge.paramss.foldLeft(sel)((fun, vparams) => Apply(fun, vparams map Ident))

          maybeWrap(bridgingCall)
      }
      DefDef(bridge, rhs)
    }
  }

  /** For `this` in a trait whose self type erases to a class that derives from the owner of `member`, that class;
   *  otherwise `NoType`.
   *
   *  A trait's `this` erases to the trait, so selecting a member of another part of the self type needs a cast. The
   *  cast to `owner` gives `checkcast Owner; invokeinterface Owner.m`; when the self type erases to a class, casting
   *  to it gives a cheaper class check and lets the backend use `invokevirtual`, as it already does for the same
   *  selection through an inner class's `$outer`, which is typed with the erased self type. In a cake whose traits
   *  are self-typed with its class (`self: SymbolTable =>`), the interface checks on a class with many interfaces
   *  are slow once its secondary supers table is full (JDK 23+) and its type profile is polluted.
   *
   *  Only members of traits are retargeted: a member of a class is already called with `invokevirtual`.
   *
   *  The JVM resolves `Cls.m` in the superclasses of `Cls` before its interfaces. If `Cls` has no forwarder for `m`
   *  (the member is abstract, or `-Xmixin-force-forwarders:false`), resolution stops at the first superclass that
   *  declares a method with the same name and descriptor. Unless that method is a public instance method, the call
   *  fails to link (a private or a static method) or may fail the access check (a protected one), so we cast to the
   *  owner. If no superclass declares one, resolution continues in the interfaces and finds `m`. Scala classes have
   *  no static methods and only their private methods are not public in bytecode. The private methods of a Java
   *  class are not entered by the classfile parser, so a Java superclass other than `Object` that declares no
   *  public method with the descriptor may still declare a private one.
   */
  private def selfTypeClass(qual: Tree, member: Symbol, context: => Context): Type = qual match {
    case This(_) if qual.symbol.isTrait && member.owner.isTrait =>
      val selfErased = erasure(qual.symbol)(qual.symbol.typeOfThis)
      val cls = selfErased.typeSymbol
      lazy val memberErased = erasure(member)(member.info)
      def sameDescriptor(owner: Symbol) =
        owner.info.decl(member.name).alternatives.filter(alt => alt.isMethod && (erasure(alt)(alt.info) matches memberErased))
      @tailrec def resolvesToMember(c: Symbol): Boolean =
        if (c == NoSymbol) true
        else if (c.isJavaDefined && sameDescriptor(c.companionModule.moduleClass).nonEmpty) false
        else sameDescriptor(c) match {
          case alt :: _ => if (c.isJavaDefined) alt.isPublic else !alt.isPrivate
          case Nil      => (c == ObjectClass || !c.isJavaDefined) && resolvesToMember(c.superClass)
        }
      if (cls.isClass && !cls.isTrait && cls.isSubClass(member.owner) && isJvmAccessible(cls, context) && resolvesToMember(cls)) selfErased
      else NoType
    case _ => NoType
  }

  /** How erasure adapts the qualifier of a selection or a cast once the qualifier has its erased
   *  type: the rules of `Eraser.adaptMember`, shared with `ErasureRetyper`, which types qualifiers
   *  its own way.
   */
  abstract class ErasureTypeAdapter extends TypeAdapter {
    /** The context deciding whether a Java-defined class is accessible in bytecode. */
    protected def accessContext: Context

    /** A member of `Any` selected after erasure becomes the corresponding member of `Object`. */
    final def redirectAnyMember(sel: Select): Select = {
      val sym = sel.symbol
      if (sym == Any_asInstanceOf) atPos(sel.pos)(Select(sel.qualifier, Object_asInstanceOf))
      else if (sym == Any_isInstanceOf) atPos(sel.pos)(Select(sel.qualifier, Object_isInstanceOf))
      else if (sym.owner == AnyClass) atPos(sel.pos)(Select(sel.qualifier, getMember(ObjectClass, sym.name)))
      else sel
    }

    /** `sel`, a selection of a member of a class other than `Any`, on its typed qualifier `qual0`:
     *
     *   - a primitive qualifier of a reference member, or a value-class qualifier, is boxed;
     *   - a reference qualifier of a member of a primitive class is unboxed;
     *   - a qualifier whose erased class does not define the member, or is not accessible in
     *     bytecode, is cast to the member's owner (or to the class in a `QualTypeSymAttachment`,
     *     see `preErase`). Selections from `super` cannot be cast; `typedSelect` handles them.
     */
    final def adaptQualifier(sel: Select, qual0: Tree): Select = {
      val sym = sel.symbol
      val isPrimitive = isPrimitiveValueType(qual0.tpe)
      val qual1 =
        if (isPrimitive && !isPrimitiveValueMember(sym) || isErasedValueType(qual0.tpe)) box(qual0)
        else if (!isPrimitive && isPrimitiveValueMember(sym)) unbox(qual0, sym.owner.tpe)
        else qual0

      def selectFrom(fromQual: Tree) = treeCopy.Select(sel, fromQual, sel.name)

      if (isPrimitiveValueMember(sym) && !isPrimitiveValueType(qual1.tpe)) {
        sel.symbol = NoSymbol
        selectFrom(qual1)
      } else if (isMethodTypeWithEmptyParams(qual1.tpe)) { // see also adaptToType in TypeAdapter
        assert(qual1.symbol.isStable, qual1.symbol)
        adaptQualifier(sel, applyMethodWithEmptyParams(qual1))
      } else if (!qual1.isInstanceOf[Super] && (!isJvmAccessible(qual1.tpe.typeSymbol, accessContext) || !qual1.tpe.typeSymbol.isSubClass(sym.owner))) {
        // A selection requires a cast:
        //   - In `(foo: Option[String]).get.trim`, the qualifier has type `Object`. We cast
        //     to the owner of `trim` (`String`), unless the owner is a non-accessible Java
        //     class, in which case a `QualTypeSymAttachment` is present (see below).
        //   - In `a.b().c()`, the qualifier `a.b()` may have an accessible type `X` before
        //     erasure, but a non-accessible type `Y` after erasure (scala/bug#10450). Again
        //     we cast to the owner of `c`, or, if that is not accessible either, to the
        //     class stored in the `QualTypeSymAttachment`.
        //   - In a trait, `this` has the trait's type, so selecting a member of another part of
        //     the self type needs a cast. If the owner is a trait and the self type erases to a
        //     class that derives from it, we may cast to that class instead (see `selfTypeClass`).
        //
        // A `QualTypeSymAttachment` is present if the selected member's owner is not an
        // accessible (java-defined) class, see `preErase`.
        //
        // Selections from `super` are not handled here because inserting a cast would not be
        // legal code. Instead there's a special case in `typedSelectInternal`.
        val qualTpe = sel.getAndRemoveAttachment[QualTypeSymAttachment] match {
          case Some(a) => a.sym.tpe
          case None => selfTypeClass(qual1, sym, accessContext).orElse(sym.owner.tpe)
        }
        selectFrom(cast(qual1, qualTpe))
      } else selectFrom(qual1)
    }

    /** Is `x.asInstanceOf[T]` an unboxing after erasure? */
    final def isUnboxingCast(targ: Tree): Boolean = isPrimitiveValueType(targ.tpe) || isErasedValueType(targ.tpe)

    /** `x.asInstanceOf[T]` for a primitive or value class `T`, `qual1` being `x` typed as `Object`.
     *  Unboxing `null` to a value class with a reference underlying type gives `null`: that test is
     *  built untyped and typed with `typedSynthetic`.
     */
    final def unboxingCast(qual1: Tree, targ: Tree, owner: Symbol, fresh: FreshNameCreator)(typedSynthetic: Tree => Tree): Tree = {
      // !!! Make pending/run/t5866b.scala work. The fix might be here and/or in unbox1.
      val noNullCheckNeeded = targ.tpe match {
        case ErasedValueType(_, underlying) => isPrimitiveValueType(underlying)
        case _ => true
      }
      if (noNullCheckNeeded) unbox(qual1, targ.tpe)
      else typedSynthetic {
        gen.evalOnce(qual1, owner, fresh) { qual =>
          If(Apply(Select(qual(), nme.eq), List(Literal(Constant(null)) setType NullTpe)),
             Literal(Constant(null)) setType targ.tpe,
             unbox(qual(), targ.tpe))
        }
      }
    }

    /** `x.isInstanceOf[V]` for a value class `V` tests for the class, not its erased value type. */
    final def eraseValueClassTypeTest(targ: Tree): Unit = targ.tpe match {
      case ErasedValueType(clazz, _) => targ.setType(clazz.tpe)
      case _ =>
    }

    /** Does the body of this constructor get calls to its class's mixin constructors? */
    final def isPrimaryConstructorDef(ddef: DefDef): Boolean = {
      val sym = ddef.symbol
      sym.isClassConstructor && sym.isPrimaryConstructor && sym.owner != ArrayClass
    }

    final def withMixinConstructorCalls(ddef: DefDef): DefDef =
      deriveDefDef(ddef)(addMixinConstructorCalls(_, ddef.symbol.owner))

    /** `templ` with the erased parents of its class, and no self type. */
    final def withErasedParents(templ: Template): Template = {
      val parents1 = templ.symbol.owner.info.parents map (t => TypeTree(t) setPos templ.pos)
      treeCopy.Template(templ, parents1, noSelfType, templ.body)
    }

    /** What erasure does to `tree1`, the typed form of `original`, before adapting it to its
     *  expected type: a lambda gets its erased SAM type; the branches of a conditional, match or
     *  try are adapted to the result type (a `Block(_, LabelDef)` was typed without an expected
     *  type, to keep patmat's tree shapes); an overloaded reference drops the alternatives that
     *  erasure made identical.
     */
    final def finishTyped(original: Tree, tree1: Tree): Tree = {
      def adaptCase(cdef: CaseDef): CaseDef = {
        val newCdef = deriveCaseDef(cdef)(adaptToType(_, tree1.tpe))
        newCdef setType newCdef.body.tpe
      }
      def adaptBranch(branch: Tree): Tree =
        if (branch == EmptyTree) branch else adaptToType(branch, tree1.tpe)

      tree1 match {
        case fun: Function =>
          fun.attachments.get[SAMFunction] match {
            case Some(SAMFunction(samTp, _, _)) => fun setType specialScalaErasure(samTp)
            case _ => fun
          }
        case If(cond, thenp, elsep) =>
          treeCopy.If(tree1, cond, adaptBranch(thenp), adaptBranch(elsep))
        case Match(selector, cases) =>
          treeCopy.Match(tree1, selector, cases map adaptCase)
        case Try(block, catches, finalizer) =>
          treeCopy.Try(tree1, adaptBranch(block), catches map adaptCase, finalizer)
        case Ident(_) | Select(_, _) =>
          if (tree1.symbol.isOverloaded) {
            val first = tree1.symbol.alternatives.head
            val firstTpe = first.tpe
            val sym1 = tree1.symbol.filter {
              alt => alt == first || !firstTpe.looselyMatches(alt.tpe)
            }
            if (original.symbol ne sym1) {
              tree1 setSymbol sym1 setType sym1.tpe
            }
          }
          tree1
        case _ =>
          tree1
      }
    }
  }

  /** The typer for trees built after erasure, with an `adapt` that boxes, unboxes and casts:
   *  the `localTyper` of the phases after erasure, and of `ErasureRetyper` for the trees it
   *  synthesises.
   */
  class Eraser(_context: Context) extends Typer(_context) {
    val typeAdapter = new ErasureTypeAdapter {
      def typedPos(pos: Position)(tree: Tree): Tree = Eraser.this.typedPos(pos)(tree)
      protected def accessContext: Context = Eraser.this.context
    }
    import typeAdapter._

    override private[nsc] def stabilize(tree: Tree, pre: Type, mode: Mode, pt: Type): Tree = tree

    /**  Replace member references as follows:
     *
     *   - `x == y` for == in class Any becomes `x equals y` with equals in class Object.
     *   - `x != y` for != in class Any becomes `!(x equals y)` with equals in class Object.
     *   - x.asInstanceOf[T] becomes x.$asInstanceOf[T]
     *   - x.isInstanceOf[T] becomes x.$isInstanceOf[T]
     *   - x.isInstanceOf[ErasedValueType(tref)] becomes x.isInstanceOf[tref.sym.tpe]
     *   - x.m where m is some other member of Any becomes x.m where m is a member of class Object.
     *   - x.m where x has unboxed value type T and m is not a directly translated member of T becomes T.box(x).m
     *   - x.m where x is a reference type and m is a directly translated member of value type T becomes x.TValue().m
     *   - All forms of x.m where x is a boxed type and m is a member of an unboxed class become
     *     x.m where m is the corresponding member of the boxed class.
     */
    private def adaptMember(tree: Tree): Tree = tree match {
      case Apply(ta @ TypeApply(sel @ Select(qual, name), targ :: Nil), List())
      if tree.symbol == Any_asInstanceOf =>
        val qual1 = typedQualifier(qual, NOmode, ObjectTpe) // need to have an expected type, see #3037
        if (isUnboxingCast(targ)) unboxingCast(qual1, targ, context.owner, fresh)(typed(_))
        else treeCopy.Apply(tree, treeCopy.TypeApply(ta, treeCopy.Select(sel, qual1, name), List(targ)), List())
      case Apply(TypeApply(Select(_, _), List(targ)), List())
      if tree.symbol == Any_isInstanceOf =>
        eraseValueClassTypeTest(targ)
        tree
      case sel @ Select(_, name) =>
        if (sel.symbol == NoSymbol) sel
        else if (name == nme.CONSTRUCTOR) {
          if (sel.symbol.owner == AnyValClass) sel.symbol = ObjectClass.primaryConstructor
          sel
        } else {
          val sel1 = redirectAnyMember(sel)
          adaptQualifier(sel1, typedQualifier(sel1.qualifier))
        }
      case _ =>
        tree
    }

    /** A replacement for the standard typer's adapt method.
     */
    override protected def adapt(tree: Tree, mode: Mode, pt: Type, original: Tree = EmptyTree): Tree =
      adaptToType(tree, pt)

    /** A replacement for the standard typer's `typed1` method.
     */
    override def typed1(tree: Tree, mode: Mode, pt: Type): Tree = {
      val tree1 = try
        tree match {
          case ddef: DefDef if isPrimaryConstructorDef(ddef) =>
            super.typed1(withMixinConstructorCalls(ddef), mode, pt) // (3)
          case templ: Template =>
            super.typed1(withErasedParents(templ), mode, pt)
          case tree @ Block(_, _: LabelDef) =>
            // Push adaptations out to the block to preserve patmat tree shapes.
            // This helps the back-end to generate better code.
            super.typed1(adaptMember(tree), mode, WildcardType)
          case _ =>
            super.typed1(adaptMember(tree), mode, pt)
        }
      catch {
        case er: TypeError =>
          Console.println("exception when typing " + tree+"/"+tree.getClass)
          Console.println(er.msg + " in file " + context.owner.sourceFile)
          er.printStackTrace
          abort("unrecoverable error")
        case ex: Exception =>
          try Console.println(s"exception when typing $tree") catch identity: @nowarn
          throw ex
      }
      finishTyped(tree, tree1)
    }
  }

  /** The erasure transformer */
  class ErasureTransformer(unit: CompilationUnit) extends AstTransformer {
    import overridingPairs.PairsCursor

    private def doubleDefError(pair: SymbolPair): Unit = {
      import pair._

      if (!pair.isErroneous) {
        val what = (
          if (low.owner == high.owner) "double definition"
          else if (low.owner == base) "name clash between defined and inherited member"
          else "name clash between inherited members"
        )
        val when =
          if (exitingRefchecks(lowType matches highType)) ""
          else s" after erasure: ${exitingPostErasure(highType)}"

        reporter.error(pos,
          sm"""|$what:
               |${exitingRefchecks(highString)} and
               |${exitingRefchecks(lowString)}
               |have same type$when"""
        )
      }
      low setInfo ErrorType
    }

    private def sameTypeAfterErasure(sym1: Symbol, sym2: Symbol) =
      exitingPostErasure(sym1.info =:= sym2.info) && !sym1.isMacro && !sym2.isMacro

    /** TODO - adapt SymbolPairs so it can be used here. */
    private def checkNoDeclaredDoubleDefs(base: Symbol): Unit = {
      val decls = base.info.decls

      // scala/bug#8010 force infos, otherwise makeNotPrivate in ExplicitOuter info transformer can trigger
      //         a scope rehash while were iterating and we can see the same entry twice!
      //         Inspection of SymbolPairs (the basis of OverridingPairs), suggests that it is immune
      //         from this sort of bug as it copies the symbols into a temporary scope *before* any calls to `.info`,
      //         ie, no variant of it calls `info` or `tpe` in `SymbolPair#exclude`.
      //
      //         Why not just create a temporary scope here? We need to force the name changes in any case before
      //         we do these checks, so that we're comparing same-named methods based on the expanded names that actually
      //         end up in the bytecode.
      exitingPostErasure(decls.foreach(_.info))

      var e = decls.elems
      while (e ne null) {
        if (e.sym.isTerm) {
          var e1 = decls lookupNextEntry e
          while (e1 ne null) {
            assert(e.sym ne e1.sym, s"Internal error: encountered ${e.sym.debugLocationString} twice during scope traversal. This might be related to scala/bug#8010.")
            if (sameTypeAfterErasure(e.sym, e1.sym))
              doubleDefError(new SymbolPair(base, e.sym, e1.sym))

            e1 = decls lookupNextEntry e1
          }
        }
        e = e.next
      }
    }

    private class DoubleDefsCursor(root: Symbol) extends PairsCursor(root) {
      // specialized members have no type history before 'specialize', causing double def errors for curried defs
      override def exclude(sym: Symbol): Boolean = (
           sym.isType
        || super.exclude(sym)
        || !sym.hasTypeAt(currentRun.refchecksPhase.id)
      )
      override def matches(high: Symbol) = !high.isPrivate
    }

    /** Emit an error if there is a double definition. This can happen if:
     *
     *  - A template defines two members with the same name and erased type.
     *  - A template defines and inherits two members `m` with different types,
     *    but their erased types are the same.
     *  - A template inherits two members `m` with different types,
     *    but their erased types are the same.
     */
    private def checkNoDoubleDefs(root: Symbol): Unit = {
      checkNoDeclaredDoubleDefs(root)
      def isErasureDoubleDef(pair: SymbolPair) = {
        import pair._
        log(s"Considering for erasure clash:\n$pair")
        !overridingPairs.matchedAtRefchecks(root, low, high) &&
          sameTypeAfterErasure(low, high) &&
          !exitingRefchecks(lowType matches highType)
      }
      (new DoubleDefsCursor(root)).iterator filter isErasureDoubleDef foreach doubleDefError
      overridingPairs.forgetMatchedAtRefchecks(root)
    }

    /**  Add bridge definitions to a template. This means:
     *
     *   If there is a concrete member `m` which overrides a member in a base
     *   class of the template, and the erased types of the two members differ,
     *   and the two members are not inherited or defined by some parent class
     *   of the template, then a bridge from the overridden member `m1` to the
     *   member `m0` is added. The bridge has the erased type of `m1` and
     *   forwards to `m0`.
     *
     *   No bridge is added if there is already a bridge to `m0` with the erased
     *   type of `m1` in the template.
     */
    private def bridgeDefs(owner: Symbol): (List[Tree], immutable.Set[Symbol]) = {
      assert(phase == currentRun.erasurePhase, phase)
      new GenerateBridges(unit, owner).generate()
    }

    def addBridgesToTemplate(stats: List[Tree], base: Symbol): List[Tree] =
      if (base.isTrait) stats
      else {
        val (bridges, toBeRemoved) = bridgeDefs(base)
        if (bridges.isEmpty) stats
        else (stats filterNot (stat => toBeRemoved contains stat.symbol)) ::: bridges
      }

    def addBridgesToLambda(lambdaClass: Symbol): Unit = {
      assert(phase == currentRun.erasurePhase, phase)
      assert(lambdaClass.isClass, lambdaClass)
      new EnterBridges(unit, lambdaClass).computeAndEnter()
    }

    /**  Rewrite trees, at phase erasure, before they are retyped with erased types.
     *   This entails the following:
     *
     *   - Remove all type parameters in class and method definitions.
     *   - Remove all abstract and alias type definitions.
     *   - Remove all type applications other than those involving a type test or cast.
     *   - Remove all empty trees in statements and definitions in a PackageDef.
     *   - Check that there are no double definitions in a template.
     *   - Add bridge definitions to a template.
     *   - Replace all types in type nodes and the EmptyTree object by their erasure.
     *     Type nodes of type Unit representing result types of methods are left alone.
     *   - Remove all instance creations new C(arg) where C is an inlined class.
     *   - Reset all other type attributes to null, thus enforcing a retyping.
     *
     *   `ErasureRetyper` applies this to each tree as it enters it (`preEraseNode`), and to whole
     *   subtrees that `preErase` synthesises (`transform`).
     */
    class PreEraseTransformer(initLocalTyper: analyzer.Typer) extends TypingTransformer(initLocalTyper) {
      // Work around some incomplete path unification :( there are similar casts in SpecializeTypes
      def context: Context = localTyper.context.asInstanceOf[Context]

      // TODO: since the spec defines instanceOf checks in terms of pattern matching,
      // this extractor should share code with TypeTestTreeMaker. The corresponding
      // code is somewhat buried in and entangled with the pattern matching mechanics
      // which makes this fiddly to do now.
      object SingletonInstanceCheck {
        def unapply(pt: Type): Option[(TermSymbol, Tree)] = {
          def containsSingleton(tp: Type): Boolean =
            tp.dealias match {
              case SingleType(_, _) | ConstantType(_) | ThisType(_) | SuperType(_, _) => true
              case RefinedType(parents, _) => parents.exists(containsSingleton)
              case _ => false
            }
          if(containsSingleton(pt)) {
            val cmpOp  = if (pt.typeSymbol.isSubClass(AnyValClass)) Any_equals else Object_eq
            val cmpArg = gen.mkAttributedQualifier(pt)
            Some((cmpOp, cmpArg))
          } else None
        }
      }

      private def preEraseNormalApply(tree: Apply) = {
        val fn = tree.fun
        val args = tree.args

        def qualifier = fn match {
          case Select(qual, _)               => qual
          case TypeApply(Select(qual, _), _) => qual
          case x                             => throw new MatchError(x)
        }

        // TODO: this should share logic with TypeTestTreeMaker in the pattern matcher,
        // since `x.isInstanceOf[T]` is specified as the pattern match. The corresponding
        // code is somewhat buried in and entangled with the pattern matching mechanics
        // which makes this fiddly to do now.
        def preEraseAsInstanceOf = {
          (fn: @unchecked) match {
            case TypeApply(Select(qual, _), List(targ)) =>
              targ.tpe match {
                case argTp if qual.tpe <:< argTp =>
                  atPos(tree.pos) { Typed(qual, TypeTree(argTp)) }
                case argTp if isNumericValueClass(qual.tpe.typeSymbol) && isNumericValueClass(argTp.typeSymbol) =>
                  atPos(tree.pos)(numericConversion(qual, argTp.typeSymbol))
                case _ =>
                  tree
              }
          }
          // todo: also handle the case where the singleton type is buried in a compound
        }

        // TODO: this should share logic with TypeTestTreeMaker in the pattern matcher,
        // since `x.isInstanceOf[T]` is specified as the pattern match. The corresponding
        // code is somewhat buried in and entangled with the pattern matching mechanics
        // which makes this fiddly to do now.
        // `x match { case _: T => true case _ => false }` (modulo numeric conversion)
        def preEraseIsInstanceOf = {
          fn match {
            case TypeApply(sel @ Select(qual, name), List(targ)) =>
              if (qual.tpe != null && isPrimitiveValueClass(qual.tpe.typeSymbol) && targ.tpe != null && targ.tpe <:< AnyRefTpe)
                reporter.error(sel.pos, "isInstanceOf cannot test if value types are references.")

              def mkIsInstanceOf(q: () => Tree)(tp: Type): Tree =
                Apply(
                  TypeApply(
                    Select(q(), Object_isInstanceOf) setPos sel.pos,
                    List(TypeTree(tp) setPos targ.pos)) setPos fn.pos,
                  List()) setPos tree.pos
              targ.tpe match {
                case SingletonInstanceCheck(cmpOp, cmpArg) =>
                  atPos(tree.pos) { Apply(Select(cmpArg, cmpOp), List(qual)) }
                case RefinedType(parents, decls) if (parents.lengthIs >= 2) =>
                  gen.evalOnce(qual, currentOwner, localTyper.fresh) { q =>
                    // Optimization: don't generate isInstanceOf tests if the static type
                    // conforms, because it always succeeds.  (Or at least it had better.)
                    // At this writing the pattern matcher generates some instance tests
                    // involving intersections where at least one parent is statically known true.
                    // That needs fixing, but filtering the parents here adds an additional
                    // level of robustness (in addition to the short term fix.)
                    val parentTests = parents filterNot (qual.tpe <:< _)

                    if (parentTests.isEmpty) Literal(Constant(true))
                    else atPos(tree.pos) {
                      parentTests map mkIsInstanceOf(q) reduceRight gen.mkAnd
                    }
                  }
                case TypeRef(_, SingletonClass, _) =>
                  atPos(tree.pos) {
                    if(qual.tpe <:< AnyRefTpe)
                      Apply(Select(qual, Object_ne), List(Literal(Constant(null)) setType NullTpe))
                    else
                      Literal(Constant(true))
                  }
                case _ => tree
              }
            case _ => tree
          }
        }

        if (fn.symbol == Any_asInstanceOf) {
          preEraseAsInstanceOf
        } else if (fn.symbol == Any_isInstanceOf) {
          preEraseIsInstanceOf
        } else if (fn.symbol.isOnlyRefinementMember) {
          // !!! Another spot where we produce overloaded types (see test pos/t6301)
          log(s"${fn.symbol.fullLocationString} originates in refinement class - call will be implemented via reflection.")
          ApplyDynamic(qualifier, args) setSymbol fn.symbol setPos tree.pos
        } else if (fn.symbol.isMethodWithExtension && !fn.symbol.tpe.isErroneous) {
          Apply(gen.mkAttributedRef(extensionMethods.extensionMethod(fn.symbol)), qualifier :: args)
        } else {
          tree
        }
      }

      private def preEraseApply(tree: Apply) = {
        tree.fun match {
          case TypeApply(fun @ Select(qual, name), args @ List(arg))
          if isTypeTestSymbol(fun.symbol) &&
              unboundedGenericArrayLevel(arg.tpe) > 0 => // !!! todo: simplify by having GenericArray also extract trees
            val level = unboundedGenericArrayLevel(arg.tpe)
            def isArrayTest(arg: Tree) =
              gen.mkRuntimeCall(nme.isArray, List(arg, Literal(Constant(level))))

            global.typer.typedPos(tree.pos) {
              if (level == 1) isArrayTest(qual)
              else gen.evalOnce(qual, currentOwner, localTyper.fresh) { qual1 =>
                gen.mkAnd(
                  gen.mkMethodCall(
                    qual1(),
                    fun.symbol,
                    List(specialErasure(fun.symbol)(arg.tpe)),
                    Nil
                  ),
                  isArrayTest(qual1())
                )
              }
            }
          case fn @ Select(qual, name) =>
            val args = tree.args
            if (fn.symbol.owner == ArrayClass) {
              // Have to also catch calls to abstract types which are bounded by Array.
              if (unboundedGenericArrayLevel(qual.tpe.widen) == 1 || qual.tpe.typeSymbol.isAbstractType) {
                // convert calls to apply/update/length on generic arrays to
                // calls of ScalaRunTime.array_xxx method calls
                global.typer.typedPos(tree.pos) {
                  val arrayMethodName = name match {
                    case nme.apply  => nme.array_apply
                    case nme.length => nme.array_length
                    case nme.update => nme.array_update
                    case nme.clone_ => nme.array_clone
                    case _          => reporter.error(tree.pos, "Unexpected array member, no translation exists.") ; nme.NO_NAME
                  }
                  gen.mkRuntimeCall(arrayMethodName, qual :: args)
                }
              } else {
                // store exact array erasure in map to be retrieved later when we might
                // need to do the cast in adaptMember
                // Note: No specialErasure needed here because we simply cast, on
                // elimination of SelectFromArray, no boxing or unboxing is done there.
                treeCopy.Apply(
                  tree,
                  SelectFromArray(qual, name, erasure(tree.symbol)(qual.tpe)).copyAttrs(fn),
                  args)
              }
            }
            else if (args.isEmpty && interceptedMethods(fn.symbol)) {
              if (poundPoundMethods.contains(fn.symbol)) {
                // This is unattractive, but without it we crash here on ().## because after
                // erasure the ScalaRunTime.hash overload goes from Unit => Int to BoxedUnit => Int.
                // This must be because some earlier transformation is being skipped on ##, but so
                // far I don't know what.  For null we now define null.## == 0.
                def staticsCall(methodName: TermName): Tree = {
                  val newTree = gen.mkMethodCall(RuntimeStaticsModule, methodName, qual :: Nil)
                  global.typer.typed(newTree)
                }

                qual.tpe.typeSymbol match {
                  case UnitClass | NullClass                         => LIT(0)
                  case IntClass | ShortClass | ByteClass | CharClass => qual
                  case BooleanClass                                  => If(qual, LIT(true.##), LIT(false.##))
                  case LongClass                                     => staticsCall(nme.longHash)
                  case FloatClass                                    => staticsCall(nme.floatHash)
                  case DoubleClass                                   => staticsCall(nme.doubleHash)
                  case _                                             => staticsCall(nme.anyHash)
                }
              } else if (isPrimitiveValueClass(qual.tpe.typeSymbol)) {
                // Rewrite 5.getClass to ScalaRunTime.anyValClass(5)
                global.typer.typed(gen.mkRuntimeCall(nme.anyValClass, List(qual, typer.resolveClassTag(tree.pos, qual.tpe.widen))))
              } else if (primitiveGetClassMethods.contains(fn.symbol)) {
                // if we got here then we're trying to send a primitive getClass method to either
                // a) an Any, in which case Object_getClass works because Any erases to object. Or
                //
                // b) a non-primitive, e.g. because the qualifier's type is a refinement type where one parent
                //    of the refinement is a primitive and another is AnyRef. In that case
                //    we get a primitive form of _getClass trying to target a boxed value
                //    so we need replace that method name with Object_getClass to get correct behavior.
                //    See scala/bug#5568.
                tree setSymbol Object_getClass
              } else {
                devWarning(s"The symbol '${fn.symbol}' was intercepted but didn't match any cases, that means the intercepted methods set doesn't match the code")
                tree
              }
            } else qual match {
              case New(tpt) if name == nme.CONSTRUCTOR && tpt.tpe.typeSymbol.isDerivedValueClass =>
                // println("inject derived: "+arg+" "+tpt.tpe)
                val List(arg) = args: @unchecked
                val attachment = new TypeRefAttachment(tree.tpe.asInstanceOf[TypeRef])
                InjectDerivedValue(arg) updateAttachment attachment
              case _ =>
                preEraseNormalApply(tree)
            }

          case _ =>
            preEraseNormalApply(tree)
        }
      }

      def preErase(tree: Tree): Tree = tree match {
        case tree: Apply =>
          preEraseApply(tree)

        case TypeApply(fun, args)
        if fun.symbol.owner != AnyClass
        && fun.symbol != Object_asInstanceOf
        && fun.symbol != Object_isInstanceOf
        && fun.symbol != Object_synchronized =>
          // leave all other type tests/type casts, remove all other type applications
          preErase(fun)

        case Select(qual, name) =>
          val sym = tree.symbol
          val owner = sym.owner
          if (owner.isRefinementClass) {
            sym.allOverriddenSymbols filterNot (_.owner.isRefinementClass) match {
              case overridden :: _ =>
                log(s"${sym.fullLocationString} originates in refinement class - replacing with ${overridden.fullLocationString}.")
                tree.symbol = overridden
              case Nil =>
                // Ideally this should not be reached or reachable; anything which would
                // get here should have been caught in the surrounding Apply.
                devWarning(s"Failed to rewrite reflective apply - now don't know what to do with " + tree)
                return treeCopy.Select(tree, gen.mkAttributedCast(qual, qual.tpe.widen), name)
            }
          }

          // This code may add an QualTypeSymAttachment to the Select tree. The referenced class is
          // then used in erasure type checking as the type of the Select's qualifier. This fixes
          // two situations where erasure type checking cannot assign a precise enough type.
          //
          //  - In a `super.m` selection, erasure typing assigns the type of the superclass to the
          //    Super tree. This is wrong if `m` is a member of a trait (not the superclass). A
          //    special-case in `typedSelectInternal` by default assigns m's owner in this case.
          //  - In a non-super selection, the qualifier may erase to a type that doesn't define the
          //    selected member, for example the qualifier of `(q: Option[String]).get.trim` erases
          //    to Object. Similarly, the qualifier may erase to a Java class that *does* define the
          //    selected member but is not accessible (scala/bug#10450).
          //    Erasure's `adaptMember` detects these cases and, by default, introduces a cast to
          //    the member's owner.
          //
          // In both cases, using the member's owner is not legal if the member is defined in
          // Java and the owner class is not accessible (scala/bug#7936, scala/bug#4283). In this
          // situation we store a valid class type of the qualifier in the attachment.
          //   - For `super.m`, we store a direct parent of the current class
          //   - For a non-super selection, we store the non-erased class type of the qualifier
          //
          // In addition, for `super.m` selections, we also store a direct parent of the current
          // class if `m` is defined in Java. This avoids the need for having the Java class as
          // a direct parent (scala-dev#143).
          if (qual.isInstanceOf[Super]) {
            val qualSym = accessibleOwnerOrParentDefiningMember(sym, qual.tpe.typeSymbol.parentSymbolsIterator, context) match {
              case Some(p) => p
              case None =>
                // There is no test for this warning, I have been unable to come up with an example that would trigger it.
                // In a selection `a.m`, there must be a direct parent from which `m` can be selected.
                reporter.error(tree.pos, s"Unable to emit super reference to ${sym.fullLocationString}, $owner is not accessible in ${context.enclClass.owner}")
                owner
            }

            if (sym.isJavaDefined && qualSym.isTraitOrInterface)
              requiredDirectInterfaces.getOrElseUpdate(context.enclClass.owner, mutable.Set.empty) += qualSym

            if (qualSym != owner)
              tree.updateAttachment(new QualTypeSymAttachment(qualSym))
          } else if (!isJvmAccessible(owner, context) ||
            // scala/bug#13007: isJvmAccessible is true for a protected java method, even if accessed through erased self type
            sym.isJavaDefined && sym.isProtected && !qual.tpe.typeSymbol.isSubClass(sym.owner)) {
            val qualSym = qual.tpe.typeSymbol
            if (qualSym != owner && isJvmAccessible(qualSym, context) && (definesMemberAfterErasure(qualSym, sym) || sym.overridingSymbol(qualSym).exists))
              tree.updateAttachment(new QualTypeSymAttachment(qualSym))
            else
              reporter.error(tree.pos, s"Unable to emit reference to ${sym.fullLocationString}, $owner is not accessible in ${context.enclClass.owner}")
          }

          tree

        case Template(parents, self, body) =>
          //Console.println("checking no dble defs " + tree)//DEBUG
          checkNoDoubleDefs(tree.symbol.owner)
          treeCopy.Template(tree, parents, noSelfType, addBridgesToTemplate(body, currentOwner))

        case Match(selector, cases) =>
          treeCopy.Match(tree, Typed(selector, TypeTree(selector.tpe)), cases)

        case Literal(ct) =>
          // We remove the original tree attachments in pre-erasure to free up memory
          val cleanLiteral = tree.removeAttachment[OriginalTreeAttachment]

          if (ct.tag == ClazzTag && ct.typeValue.typeSymbol != definitions.UnitClass) {
            val typeValue = ct.typeValue.dealiasWiden
            val erased = erasure(typeValue.typeSymbol) applyInArray typeValue
            treeCopy.Literal(cleanLiteral, Constant(erased))
          } else cleanLiteral

        case ClassDef(_,_,_,_) =>
          debuglog("defs of " + tree.symbol + " = " + tree.symbol.info.decls)
          copyClassDef(tree)(tparams = Nil)
        case DefDef(_,_,_,_,_,_) =>
          copyDefDef(tree)(tparams = Nil)
        case TypeDef(_, _, _, _) =>
          EmptyTree

        case fun: Function =>
          fun.attachments.get[SAMFunction] foreach {
            samf => addBridgesToLambda(samf.synthCls)
          }
          fun

        case _ =>
          tree
      }

      /** Pre-erase `tree` and clear its type, so that the retyper types it at `erasure.next`.
       *
       *  With `deep`, all of `tree` is pre-erased. Without, only `tree` and the type trees directly
       *  under it, as `ErasureRetyper` does when it enters a tree: the other children keep their
       *  pre-erasure types until the retyper enters them, so `preErase` still sees pre-erasure
       *  types wherever it looks below the node it rewrites. Trees that `preErase` synthesises
       *  (rewritten applications, bridges) are pre-erased whole either way.
       *
       *  The result keeps a type only where the retyper must not retype it: type trees, the
       *  `ApplyDynamic` of a symbol literal and references to `Array`.
       */
      private def preEraseTree(tree: Tree, deep: Boolean): Tree = {
        def children(t: Tree): Tree = if (deep) super.transform(t) else shallowChildren(t)
        // Reply to "!!! needed?" which adorned the next line: without it, build fails with:
        //   Exception in thread "main" scala.tools.nsc.symtab.Types$TypeError:
        //   value array_this is not a member of object scala.runtime.ScalaRunTime
        //
        // What the heck is array_this? See preTransformer in this file:
        //   gen.mkRuntimeCall("array_"+name, qual :: args)
        if (tree.symbol == ArrayClass && !tree.isType) tree
        else {
          val tree1 = preErase(tree)
          tree1 match {
            case TypeApply(fun, targs @ List(targ)) if (fun.symbol == Any_asInstanceOf  || fun.symbol == Object_synchronized) && targ.tpe == UnitTpe =>
              // scala/bug#9066 prevent transforming `o.asInstanceOf[Unit]` to `o.asInstanceOf[BoxedUnit]`.
              // adaptMember will then replace the call by a reference to BoxedUnit.UNIT.
              treeCopy.TypeApply(tree1, transform(fun), targs).clearType()
            case EmptyTree | TypeTree() =>
              tree1 setType specialScalaErasure(tree1.tpe)
            case ArrayValue(elemtpt, trees) =>
              treeCopy.ArrayValue(
                tree1, elemtpt setType specialScalaErasure.applyInArray(elemtpt.tpe), if (deep) trees map transform else trees).clearType()
            case ValDef(_, _, _, _) =>
              val vd1 = children(tree1).clearType().asInstanceOf[ValDef]
              vd1.tpt.tpe match {
                case FoldableConstantType(_) if !vd1.rhs.isInstanceOf[Literal] =>
                  val deconst = vd1.tpt.tpe.deconst
                  vd1.tpt setType deconst
                  tree1.symbol.setInfo(deconst)
                case _ =>
              }
              vd1
            case DefDef(_, _, _, _, tpt, _) =>
              // TODO: move this in some post-processing transform in the fields phase?
              if (fields.symbolAnnotationsTargetFieldAndGetter(tree.symbol))
                fields.dropFieldAnnotationsFromGetter(tree.symbol)

              try children(tree1).clearType()
              finally tpt setType specialErasure(tree1.symbol)(tree1.symbol.tpe).resultType
            case ApplyDynamic(_, Literal(Constant(_: Symbol)) :: _) =>
              tree
            case _: Apply if tree1 ne tree =>
              /* some Apply trees get replaced (in `preEraseApply`) with one of
               * their subtrees, which needs to be `preErase`d in its entirety,
               * not just recursed over by super.transform(). */
              transform(tree1)
            case _ =>
              children(tree1).clearType()
          }
        }
      }

      /** The children `preEraseTree` handles without `deep`: type trees, and the trees `preErase`
       *  added, which the retyper would take for already pre-erased (they have no type). */
      private def shallowChildren(tree: Tree): Tree = tree match {
        case vd @ ValDef(_, _, tpt, _)          => treeCopy.ValDef(vd, vd.mods, vd.name, eraseTypeTree(tpt), vd.rhs)
        case dd @ DefDef(_, _, _, _, tpt, _)    => treeCopy.DefDef(dd, dd.mods, dd.name, dd.tparams, dd.vparamss, eraseTypeTree(tpt), dd.rhs)
        case Typed(expr, tpt)                   => treeCopy.Typed(tree, expr, eraseTypeTree(tpt))
        case TypeApply(fun, targs)              => treeCopy.TypeApply(tree, fun, targs mapConserve eraseTypeTree)
        case New(tpt)                           => treeCopy.New(tree, eraseTypeTree(tpt))
        case Match(sel @ Typed(expr, tpt), cases) if sel.tpe eq null =>
          // the `Typed` that `preErase` wraps around a selector
          treeCopy.Match(tree, treeCopy.Typed(sel, expr, eraseTypeTree(tpt)), cases)
        case templ: Template =>
          // bridges
          treeCopy.Template(templ, templ.parents, templ.self, templ.body mapConserve (stat => if (stat.tpe eq null) transform(stat) else stat))
        case _ => tree
      }

      /** A type tree erased as `transform` erases it. Anything but a `TypeTree` comes back without
       *  a type, for the retyper to type (`untypecheck` can leave other trees in type position). */
      private def eraseTypeTree(tpt: Tree): Tree = tpt match {
        case EmptyTree => tpt
        case tt: TypeTree => tt setType specialScalaErasure(tt.tpe)
        case _ => transform(tpt)
      }

      /** `preEraseTree(tree, deep = false)`, called `exitingErasure`. Enters the erasure phase only
       *  for trees on which `preErase` does more than the default (`case _ => tree`). */
      def preEraseNode(tree: Tree): Tree = tree match {
        case _ if tree.hasSymbolField && (tree.symbol eq ArrayClass) && !tree.isType => tree
        case _: Ident | _: This | _: Block | _: If | _: LabelDef | _: Assign | _: Return | _: Throw | _: Try |
             _: CaseDef | _: Super | _: PackageDef | _: Bind | _: Alternative =>
          tree.clearType()
        case lit @ Literal(ct) if ct.tag != ClazzTag =>
          lit.removeAttachment[OriginalTreeAttachment].clearType()
        case sel @ Select(qual, _) if !qual.isInstanceOf[Super] && (sel.symbol ne NoSymbol) && {
            val sym = sel.symbol; val owner = sym.owner
            !owner.isRefinementClass && !owner.isJavaDefined && !sym.isJavaDefined } =>
          sel.clearType()
        case _ =>
          enteringErasure(preEraseTree(tree, deep = false))
      }

      /** Pre-erase the children of `tree` whole, as `transform` does. */
      def preEraseChildren(tree: Tree): Tree = enteringErasure(super.transform(tree))

      override def transform(tree: Tree): Tree = preEraseTree(tree, deep = true)
    }

    /** Erases a compilation unit in one traversal, without a `Typer`.
     *
     *  On entering a tree, it pre-erases it (`preEraseNode`). On leaving, the tree gets its erased
     *  type from the erased info of its symbol and the types of its children, as `Eraser` (a
     *  `Typer` with an overridden `adapt`) would compute it, and is adapted to its expected type
     *  with `TypeAdapter.adaptToType`, where boxing, unboxing and casts are inserted. Only the trees
     *  that adaptation synthesises go through `localTyper`, which is an `Eraser`.
     *
     *  Must be created and run `exitingErasure`.
     */
    final class ErasureRetyper(root: Tree)
        extends PreEraseTransformer(newTyper(rootContextPostTyper(unit, root)).asInstanceOf[global.analyzer.Typer])
        with TypeAssigner {
      private val typeAdapter: ErasureTypeAdapter = new ErasureTypeAdapter {
        def typedPos(pos: Position)(tree: Tree): Tree = localTyper.typedPos(pos)(tree)
        protected def accessContext: Context = localTyper.context.asInstanceOf[Context]
        override protected def typedCast(tree: Tree, pt: Type): Tree = typedSyntheticCast(tree, pt)
      }
      import typeAdapter._

      def retype(tree: Tree): Tree = typed(tree, WildcardType)

      /** Every tree is visited once, by `typed`. A tree with a type is an original tree, pre-erased
       *  here on entry; after that, it has a type only if it must not be retyped (see
       *  `preEraseTree`). Trees synthesised by adaptation (box, unbox, cast) come back typed and
       *  are not revisited.
       */
      override protected def enter(tree: Tree): Tree = if (tree.tpe ne null) preEraseNode(tree) else tree

      /** `Eraser.adapt`. */
      override protected def adapt(tree: Tree, pt: Type, funMode: Boolean): Tree = adaptToType(tree, pt)

      /** What is left of a tree the assigner does not know is pre-erased whole and typed with `Eraser`. */
      override protected def beforeFallback(tree: Tree): Tree = preEraseChildren(tree)

      /** Analogue of `Eraser.typed1`. */
      override protected def typed1(tree: Tree, pt: Type, qualMode: Boolean): Tree = {
        val tree1 = tree match {
          case ddef: DefDef if isPrimaryConstructorDef(ddef) =>
            typedCore(withMixinConstructorCalls(ddef), pt, qualMode)
          case templ: Template =>
            typedCore(withErasedParents(templ), pt, qualMode)
          case InjectDerivedValue(arg) =>
            // A value class instance: its underlying value, with the class's erased value type.
            (tree.attachments.get[TypeRefAttachment]: @unchecked) match {
              case Some(itype) =>
                val tref = itype.tpe
                val result = typed(arg, enteringErasure(erasedValueClassArg(tref)))
                return result setType ErasedValueType(tref.sym, result.tpe)
            }
          case Block(_, _: LabelDef) =>
            // Push adaptations out to the block to preserve patmat tree shapes.
            typedCore(tree, WildcardType, qualMode)
          case _ =>
            typedCore(tree, pt, qualMode)
        }
        finishTyped(tree, tree1)
      }

      /** The tree shapes erasure types differently from `TypeAssigner`. */
      override protected def typedCore(tree: Tree, pt: Type, qualMode: Boolean): Tree = tree match {
        case sel @ Select(qual, name) =>
          if (sel.symbol == NoSymbol) typedSelect(sel, typedQualifier(qual), qualMode, pt)
          else if (name == nme.CONSTRUCTOR) {
            if (sel.symbol.owner == AnyValClass) sel.symbol = ObjectClass.primaryConstructor
            typedSelect(sel, typedQualifier(qual), qualMode, pt)
          } else {
            val sel1 = redirectAnyMember(sel)
            val qual1 = typedQualifier(sel1.qualifier)
            val sel2 = adaptQualifier(sel1, qual1)
            typedSelect(sel2, sel2.qualifier, qualMode, pt)
          }

        case sel @ SelectFromArray(qual, name, erasure) =>
          // An element access of an array of a statically known element type.
          val qual0 = typedQualifier(qual)
          val qual1 = if (qual0.tpe <:< erasure) qual0 else cast(qual0, erasure)
          typedSelect(Select(qual1, name) copyAttrs sel, qual1, qualMode, pt)

        case app @ Apply(ta: TypeApply, Nil) if app.symbol == Any_asInstanceOf || app.symbol == Any_isInstanceOf =>
          // The type argument decides how the test or cast is erased: enter the spine to erase it
          // before looking at it.
          enter(ta) match {
            case ta1 @ TypeApply(sel: Select, targs @ List(targ)) =>
              if (app.symbol == Any_isInstanceOf) {
                eraseValueClassTypeTest(targ)
                typedApply(treeCopy.Apply(app, ta1, Nil), typed(ta1, WildcardType), pt, qualMode)
              } else {
                val qual1 = typedQualifier(enter(sel).asInstanceOf[Select].qualifier, ObjectTpe) // need to have an expected type, see #3037
                if (isUnboxingCast(targ))
                  unboxingCast(qual1, targ, currentOwner, localTyper.fresh)(localTyper.typed(_))
                else {
                  val sel1 = adaptQualifier(redirectAnyMember(treeCopy.Select(sel, qual1, sel.name)), qual1)
                  val fun1 = typedTypeApply(treeCopy.TypeApply(ta1, typedSelect(sel1, sel1.qualifier, qualMode = false, WildcardType), targs), pt)
                  typedApply(treeCopy.Apply(app, fun1, Nil), fun1, pt, qualMode)
                }
              }
            case ta1 =>
              typedApply(treeCopy.Apply(app, ta1, Nil), typed(ta1, WildcardType), pt, qualMode)
          }

        case fun @ Function(vparams, body) =>
          // No expected type for the body: after erasure `FunctionN` has no type parameters, so
          // `Typer.argsResProtosFromFun` finds no result type, even for a SAM. `Delambdafy` relies on
          // an unadapted body (the call to the lifted method).
          val resProto = WildcardType
          if (fun.symbol == NoSymbol) return fallback(fun, pt, "Function-nosym")
          val (vparams1, body1) = atOwner(fun, fun.symbol) {
            (vparams.mapConserve(vd => typed(vd, WildcardType).asInstanceOf[ValDef]), typed(body, resProto))
          }
          treeCopy.Function(fun, vparams1, body1) setType FunctionClass(vparams.length).tpe

        case _ =>
          super.typedCore(tree, pt, qualMode)
      }
    }

    /** The main transform function: pre-erase and retype the tree at phase erasure.next. */
    override def transform(tree: Tree): Tree =
      exitingErasure(new ErasureRetyper(tree).retype(tree))
  }

  final def resolveAnonymousBridgeClash(sym: Symbol, bridge: Symbol): Unit = {
    // TODO reinstate this after Delambdafy generates anonymous classes that meet this requirement.
    // require(sym.owner.isAnonymousClass, sym.owner)
    log(s"Expanding name of ${sym.debugLocationString} as it clashes with bridge. Renaming deemed safe because the owner is anonymous.")
    sym.expandName(sym.owner)
    bridge.resetFlag(BRIDGE)
  }

  // exclude primitives and value classes, which need special boxing
  def isReferenceType(tp: Type) = !isErasedValueType(tp) && {
    val sym = tp.typeSymbol
    !(isPrimitiveValueClass(sym) || sym.isDerivedValueClass)
  }

  /**
   * Check if LMF can adapt between an `impl` and an `intf` signature.
   *
   * The constraints are specified here: https://docs.oracle.com/javase/8/docs/api/java/lang/invoke/LambdaMetafactory.html
   * Note that LMF can bridge between primitive types and their boxed variants. But we cannot use that because
   * unboxing `null` in Scala needs to return the zero value (LMF would produce an NPE).
   *
   * Given `samMethodType: (U1..Un)Ru` and function type T1,..., Tn => Rt (the target method created by uncurry),
   * we use the original lambda target for `implMethod: (<captured args> A1..An)Ra` if,
   * for i=1..N:
   *  Ai =:= Ui || (Ai <:< Ui <:< AnyRef)
   *  Ru =:= void || (Ra =:= Ru || (Ra <:< AnyRef, Ru <:< AnyRef))
   *
   * (We can ignore captured arguments here)
   *
   * Then we can use the target method as-is, LMF will generate a correct bridge if needed.
   *
   * Otherwise, we create an `anonfun$adapted` method in delambdafy that uses the types closest
   * to the target method that still meet the above requirements.
   */
  final def lmfAdaptationOk(implParamTypes: List[Type], implResultType: Type, intfParamTypes: List[Type], intfResultType: Type): (Boolean, Boolean) = {
    val paramsOk = implParamTypes.corresponds(intfParamTypes) { (implParamType, samParamType) =>
      implParamType =:= samParamType ||
        (isReferenceType(implParamType) && isReferenceType(samParamType) && implParamType <:< samParamType)
    }

    val resultOk = intfResultType =:= UnitTpe ||
      implResultType =:= intfResultType ||
      isReferenceType(implResultType) && isReferenceType(intfResultType)

    (paramsOk, resultOk)
  }

  /**
   * If the SAM overrides a parent method, only use LMF if it can to bridge between the two signatures.
   * Otherwise, the lambda is expanded to an anonymous class.
   */
  private def lmfOverridesOk(samSym: Symbol): Boolean = {
    val samParamTypes = exitingErasure(samSym.info.paramTypes)
    val samResult = exitingErasure(samSym.info.resultType)
    samSym.allOverriddenSymbols.forall { overridden =>
      val overriddenParamTypes = exitingErasure(overridden.info.paramTypes)
      val overriddenResult = exitingErasure(overridden.info.resultType)
      val (paramsOk, resultOk) = lmfAdaptationOk(samParamTypes, samResult, overriddenParamTypes, overriddenResult)
      paramsOk && resultOk
    }
  }

  /** Does this symbol compile to the underlying platform's notion of an interface,
    * without requiring compiler magic before it can be instantiated?
    *
    * More specifically, we're interested in whether LambdaMetaFactory can instantiate this type,
    * assuming it has a single abstract method. In other words, if we were to mix this
    * trait into a class, it should not result in any compiler-generated members having to be
    * implemented in ("mixed in to") this class (except for the SAM).
    *
    * Thus, the type must erase to a java interface, either by virtue of being defined as one,
    * or by being a trait that:
    *   - is static (explicitouter or lambdalift may add disqualifying members)
    *   - extends only other traits that compile to pure interfaces (except for Any)
    *   - has no val/var members
    *
    * TODO: can we speed this up using the INTERFACE flag, or set it correctly by construction?
    */
  final def compilesToPureInterface(tpSym: Symbol, samSym: Symbol): Boolean = {
    def ok(sym: Symbol) =
      sym.isJavaInterface ||
      sym.isTrait &&
      // Unless sym.isStatic, even if the constructor is zero-argument now, it may acquire arguments in explicit outer or lambdalift.
      // This is an impl restriction to simplify the decision of whether to expand the SAM during uncurry
      // (when we don't yet know whether it will receive an outer pointer in explicit outer or whether lambda lift will add proxies for captures).
      // When we delay sam expansion until after explicit outer & lambda lift, we could decide there whether
      // to expand sam at compile time or use LMF, and this implementation restriction could be lifted.
      sym.isStatic &&
      // HACK: this is to rule out traits with an effectful initializer.
      // The constructor only exists if the trait's template has statements.
      // Sadly, we can't be more precise without access to the tree that defines the SAM's owner.
      // Note that Scala 2 adds an `$init$` method when a trait has concrete definition (including a method).
      // `Function1`, for example, has a constructor (one that just returns).
      !sym.primaryConstructor.exists &&
      (sym.isInterface || sym.info.decls.forall(mem => mem.isMethod || mem.isType))

    // we still need to check our ancestors even if the INTERFACE flag is set, as it doesn't take inheritance into account
    ok(tpSym) && tpSym.ancestors.forall(sym => (sym eq AnyClass) || (sym eq ObjectClass) || ok(sym)) &&
    lmfOverridesOk(samSym)
  }

  // `context` is by-name, and this inlined to drop its closure: a typing transformer makes its
  // context only when asked, and only a Java-defined class needs one.
  @inline final def isJvmAccessible(cls: Symbol, context: => Context): Boolean = {
    // Phase travel necessary, isAccessible is too lax after erasure for Java-defined members, see
    // comment in its implementation.
    // A public class (`java.lang.String`, say) is accessible from anywhere: `isAccessible` finds its
    // access boundary is the root class without reading the context, so don't make one.
    !cls.isJavaDefined || cls.isPublic && cls.enclosingRootClass == rootMirror.RootClass ||
      atErasure(context.isAccessible(cls, cls.owner.thisType))
  }

  // `-Ystop-after:<phase before erasure>` leaves erasure out of the run: no phase to travel to.
  private def atErasure[T](op: => T): T =
    if (currentRun.erasurePhase == NoPhase) op else enteringErasure(op)

  /**
   * Check if a class defines a member after erasure. The phase travel is important for
   * `trait T extends AClass`: after erasure (and in bytecode), `T` has supertype `Object`, not
   * `AClass`.
   */
  final def definesMemberAfterErasure(cls: Symbol, member: Symbol): Boolean =
    atPostErasure(cls.tpe.member(member.name).alternatives.contains(member))

  private def atPostErasure[T](op: => T): T =
    if (currentRun.erasurePhase == NoPhase) op else exitingErasure(op)

  /**
   * The goal of this method is to find a class that is accessible (in bytecode) and can be used
   * to select `member`.
   * - For constructors, it returns the `member.owner`. We can assume the class is accessible: if
   *   it wasn't, the typer would have rejected the program, as the class is referenced in source.
   * - For Scala-defined members it also returns `member.owner`, all Scala-defined classes are
   *   public in bytecode.
   * - For Java-defined members we prefer a direct parent over of the owner, even if the owner is
   *   accessible. This way the owner doesn't need to be added as a direct parent, see scala-dev#143.
   */
  final def accessibleOwnerOrParentDefiningMember(member: Symbol, parents: Iterator[Symbol], context: => Context): Option[Symbol] = {
    def eraseAny(cls: Symbol) = if (cls == AnyClass || cls == AnyValClass) ObjectClass else cls

    if (member.isConstructor || !member.isJavaDefined) Some(eraseAny(member.owner))
    else parents.find { p =>
      val e = eraseAny(p)
      isJvmAccessible(e, context) && definesMemberAfterErasure(e, member)
    } orElse {
      val e = eraseAny(member.owner)
      if (isJvmAccessible(e, context)) Some(e) else None
    }
  }

  private class TypeRefAttachment(val tpe: TypeRef)
}
