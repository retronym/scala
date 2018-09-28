/* NSC -- new Scala compiler
 * Copyright 2005-2012 LAMP/EPFL
 * @author  Martin Odersky
 */


package scala.tools.nsc
package backend
package jvm

import scala.collection.{immutable, mutable}
import scala.tools.nsc.symtab._
import scala.tools.asm
import GenBCode._
import BackendReporting._

/*
 *
 *  @author  Miguel Garcia, http://lamp.epfl.ch/~magarcia/ScalaCompilerCornerReloaded/
 *  @version 1.0
 *
 */
abstract class BCodeSkelBuilder extends BCodeHelpers {
  import global._
  import bTypes._
  import coreBTypes._
  import genBCode.postProcessor.backendUtils

  /*
   * There's a dedicated PlainClassBuilder for each CompilationUnit,
   * which simplifies the initialization of per-class data structures in `genPlainClass()` which in turn delegates to `initJClass()`
   *
   * The entry-point to emitting bytecode instructions is `genDefDef()` where the per-method data structures are initialized,
   * including `resetMethodBookkeeping()` and `initJMethod()`.
   * Once that's been done, and assuming the method being visited isn't abstract, `emitNormalMethodBody()` populates
   * the ASM MethodNode instance with ASM AbstractInsnNodes.
   *
   * Given that CleanUp delivers trees that produce values on the stack,
   * the entry-point to all-things instruction-emit is `genLoad()`.
   * There, an operation taking N arguments results in recursively emitting instructions to lead each of them,
   * followed by emitting instructions to process those arguments (to be found at run-time on the operand-stack).
   *
   * In a few cases the above recipe deserves more details, as provided in the documentation for:
   *   - `genLoadTry()`
   *   - `genSynchronized()
   *   - `jumpDest` , `cleanups` , `labelDefsAtOrUnder`
   */
  abstract class PlainSkelBuilder(cunit: CompilationUnit)
    extends BCClassGen
    with    BCAnnotGen
    with    BCInnerClassGen
    with    JAndroidBuilder
    with    BCForwardersGen
    with    BCPickles
    with    BCJGenSigGen {

    // Strangely I can't find this in the asm code 255, but reserving 1 for "this"
    final val MaximumJvmParameters = 254

    // current class
    var cnode: asm.tree.ClassNode  = null
    var thisBType: ClassBType      = null

    var claszSymbol: Symbol        = null
    var isCZParcelable             = false
    var isCZStaticModule           = false

    /* ---------------- idiomatic way to ask questions to typer ---------------- */

    def paramTKs(app: Apply): List[BType] = {
      val Apply(fun, _)  = app
      val funSym = fun.symbol
      funSym.info.paramTypes map typeToBType
    }

    def symInfoTK(sym: Symbol): BType = typeToBType(sym.info)

    def tpeTK(tree: Tree): BType = typeToBType(tree.tpe)


    private def canAssignModuleInClinit(cd: ClassDef, sym: Symbol): Boolean = {
      import global.definitions._
      val parentsArePure = claszSymbol.parentSymbols.forall(sym => sym == ObjectClass || isFunctionSymbol(sym) || isAbstractFunctionSymbol(sym) || sym == definitions.SerializableClass)
      def isPureConstructor(dd: DefDef): Boolean = {
        dd.rhs match {
          case Block(stats, _) => treeInfo.isSuperConstrCall(stats.last)
          case _ => false
        }
      }
      def constructorsArePure = cd.impl.body.iterator.collect {
        case dd: DefDef if dd.symbol.isConstructor => dd
      }.forall(isPureConstructor)
      parentsArePure && constructorsArePure 
    }

    /* ---------------- helper utils for generating classes and fields ---------------- */

    def genPlainClass(cd0: ClassDef): Unit = {
      assert(cnode == null, "GenBCode detected nested methods.")

      claszSymbol       = cd0.symbol
      isCZParcelable    = isAndroidParcelableClass(claszSymbol)
      isCZStaticModule  = isStaticModuleClass(claszSymbol)
      thisBType         = classBTypeFromSymbol(claszSymbol)

      cnode = new ClassNode1()

      initJClass(cnode)
      val cd = if (isCZStaticModule) {
        // Move statements from the primary constructor following the superclass constructor call to
        // a newly synthesised tree representing the "<clinit>", which also assigns the MODULE$ field.
        // Because the assigments to both the module instance fields, and the fields of the module itself
        // are in the <clinit>, these fields can be static + final.

        // TODO should we do this transformation earlier, say in Constructors? Or would that just cause
        // pain for scala-{js, native}?

        for (f <- fieldSymbols(claszSymbol)) {
          f.setFlag(Flags.STATIC)
        }
        val constructorDefDef = treeInfo.firstConstructor(cd0.impl.body).asInstanceOf[DefDef]
        val (uptoSuperStats, remainingConstrStats) = treeInfo.splitAtSuper(constructorDefDef.rhs.asInstanceOf[Block].stats, classOnly = true)
        val clInitSymbol = claszSymbol.newMethod(TermName("<clinit>"), claszSymbol.pos, Flags.STATIC).setInfo(NullaryMethodType(definitions.UnitTpe))
        val moduleField = claszSymbol.newValue(nme.MODULE_INSTANCE_FIELD, claszSymbol.pos, Flags.STATIC | Flags.PRIVATE).setInfo(claszSymbol.tpeHK)
        val callConstructor = NewFromConstructor(claszSymbol.primaryConstructor).setType(claszSymbol.tpeHK)
        val assignModuleField = Assign(global.gen.mkAttributedRef(moduleField).setType(claszSymbol.tpeHK), callConstructor).setType(definitions.UnitTpe)
        val remainingConstrStatsSubst = remainingConstrStats.map(_.substituteThis(claszSymbol, global.gen.mkAttributedRef(claszSymbol.sourceModule)).changeOwner(claszSymbol.primaryConstructor -> clInitSymbol))
        val clinit = DefDef(clInitSymbol, Block(assignModuleField :: remainingConstrStatsSubst, Literal(Constant(())).setType(definitions.UnitTpe)).setType(definitions.UnitTpe))
        deriveClassDef(cd0)(tmpl => deriveTemplate(tmpl)(body =>
          clinit :: body.map {
            case `constructorDefDef` => copyDefDef(constructorDefDef)(rhs = Block(uptoSuperStats, constructorDefDef.rhs.asInstanceOf[Block].expr))
            case tree => tree
          }
        ))
      } else cd0

      val hasStaticCtor = methodSymbols(cd) exists (_.isStaticConstructor)
      if (!hasStaticCtor && isCZParcelable) fabricateStaticInitAndroid()

      val optSerial: Option[Long] = serialVUID(claszSymbol)
      /* serialVersionUID can't be put on interfaces (it's a private field).
       * this is fine because it wouldn't do anything anyways. */
      if (optSerial.isDefined && !claszSymbol.isTrait) {
        addSerialVUID(optSerial.get, cnode)
      }

      addClassFields()

      gen(cd.impl)

      cnode.visitAttribute(thisBType.inlineInfoAttribute.get)

      if (AsmUtils.traceClassEnabled && cnode.name.contains(AsmUtils.traceClassPattern))
        AsmUtils.traceClass(cnode)

      assert(cd.symbol == claszSymbol, "Someone messed up BCodePhase.claszSymbol during genPlainClass().")
    } // end of method genPlainClass()

    /*
     * must-single-thread
     */
    private def initJClass(jclass: asm.ClassVisitor): Unit = {

      val bType = classBTypeFromSymbol(claszSymbol)
      val superClass = bType.info.get.superClass.getOrElse(ObjectRef).internalName
      val interfaceNames = bType.info.get.interfaces.map(_.internalName)

      val flags = javaFlags(claszSymbol)

      val thisSignature = getGenericSignature(claszSymbol, claszSymbol.owner)
      cnode.visit(backendUtils.classfileVersion.get, flags,
                  thisBType.internalName, thisSignature,
                  superClass, interfaceNames.toArray)

      if (emitSource) {
        cnode.visitSource(cunit.source.toString, null /* SourceDebugExtension */)
      }

      enclosingMethodAttribute(claszSymbol, internalName, methodBTypeFromSymbol(_).descriptor) match {
        case Some(EnclosingMethodEntry(className, methodName, methodDescriptor)) =>
          cnode.visitOuterClass(className, methodName, methodDescriptor)
        case _ => ()
      }

      val ssa = getAnnotPickle(thisBType.internalName, claszSymbol)
      cnode.visitAttribute(if (ssa.isDefined) pickleMarkerLocal else pickleMarkerForeign)
      emitAnnotations(cnode, claszSymbol.annotations ++ ssa)

      if (isCZStaticModule || isCZParcelable) {

        if (isCZStaticModule) { addModuleInstanceField() }

      } else {

        if (!settings.noForwarders) {
          val lmoc = claszSymbol.companionModule
          // add static forwarders if there are no name conflicts; see bugs #363 and #1735
          if (lmoc != NoSymbol) {
            // it must be a top level class (name contains no $s)
            val isCandidateForForwarders = {
              exitingPickler { !(lmoc.name.toString contains '$') && lmoc.hasModuleFlag && !lmoc.isNestedClass }
            }
            if (isCandidateForForwarders) {
              log(s"Adding static forwarders from '$claszSymbol' to implementations in '$lmoc'")
              addForwarders(cnode, thisBType.internalName, lmoc.moduleClass)
            }
          }
        }

      }

      // the invoker is responsible for adding a class-static constructor.

    } // end of method initJClass

    /*
     * can-multi-thread
     */
    private def addModuleInstanceField(): Unit = {
      // TODO confirm whether we really don't want ACC_SYNTHETIC nor ACC_DEPRECATED
      val mods = GenBCode.PublicStaticFinal
      val fv =
        cnode.visitField(mods,
                         strMODULE_INSTANCE_FIELD,
                         thisBType.descriptor,
                         null, // no java-generic-signature
                         null  // no initial value
        )

      fv.visitEnd()
    }

    protected def assignModuleInstanceField(meth: asm.MethodVisitor): Unit = {
      meth.visitFieldInsn(asm.Opcodes.PUTSTATIC, thisBType.internalName, strMODULE_INSTANCE_FIELD, thisBType.descriptor)
    }
    /*
     * must-single-thread
     */
    private def fabricateStaticInitAndroid(): Unit = {

      val clinit: asm.MethodVisitor = cnode.visitMethod(
        GenBCode.PublicStatic, // TODO confirm whether we really don't want ACC_SYNTHETIC nor ACC_DEPRECATED
        CLASS_CONSTRUCTOR_NAME,
        "()V",
        null, // no java-generic-signature
        null  // no throwable exceptions
      )
      clinit.visitCode()

      if (isCZParcelable) { legacyAddCreatorCode(clinit, cnode, thisBType.internalName) }

      clinit.visitInsn(asm.Opcodes.RETURN)
      clinit.visitMaxs(0, 0) // just to follow protocol, dummy arguments
      clinit.visitEnd()
    }

    def addClassFields(): Unit = {
      for (f <- fieldSymbols(claszSymbol)) {
        val javagensig = getGenericSignature(f, claszSymbol)
        val flags = javaFieldFlags(f)

        val jfield = new asm.tree.FieldNode(
          flags,
          f.javaSimpleName.toString,
          symInfoTK(f).descriptor,
          javagensig,
          null // no initial value
        )
        cnode.fields.add(jfield)
        emitAnnotations(jfield, f.annotations)
      }

    } // end of method addClassFields()

    // current method
    var mnode: asm.tree.MethodNode = null
    var jMethodName: String        = null
    var isMethSymStaticCtor        = false
    var returnType: BType          = null
    var methSymbol: Symbol         = null
    // in GenASM this is local to genCode(), ie should get false whenever a new method is emitted (including fabricated ones eg addStaticInit())
    var isModuleInitialized        = false
    // used by genLoadTry() and genSynchronized()
    var earlyReturnVar: Symbol     = null
    var shouldEmitCleanup          = false
    // line numbers
    var lastEmittedLineNr          = -1

    object bc extends JCodeMethodN {
      override def jmethod = PlainSkelBuilder.this.mnode
    }

    /* ---------------- Part 1 of program points, ie Labels in the ASM world ---------------- */

    /*
     *  A jump is represented as an Apply node whose symbol denotes a LabelDef, the target of the jump.
     *  The `jumpDest` map is used to:
     *    (a) find the asm.Label for the target, given an Apply node's symbol;
     *    (b) anchor an asm.Label in the instruction stream, given a LabelDef node.
     *  In other words, (a) is necessary when visiting a jump-source, and (b) when visiting a jump-target.
     *  A related map is `labelDef`: it has the same keys as `jumpDest` but its values are LabelDef nodes not asm.Labels.
     *
     */
    var jumpDest: immutable.Map[ /* LabelDef */ Symbol, asm.Label ] = null
    def programPoint(labelSym: Symbol): asm.Label = {
      assert(labelSym.isLabel, s"trying to map a non-label symbol to an asm.Label, at: ${labelSym.pos}")
      jumpDest.getOrElse(labelSym, {
        val pp = new asm.Label
        jumpDest += (labelSym -> pp)
        pp
      })
    }

    /*
     *  A program point may be lexically nested (at some depth)
     *    (a) in the try-clause of a try-with-finally expression
     *    (b) in a synchronized block.
     *  Each of the constructs above establishes a "cleanup block" to execute upon
     *  both normal-exit, early-return, and abrupt-termination of the instructions it encloses.
     *
     *  The `cleanups` LIFO queue represents the nesting of active (for the current program point)
     *  pending cleanups. For each such cleanup an asm.Label indicates the start of its cleanup-block.
     *  At any given time during traversal of the method body,
     *  the head of `cleanups` denotes the cleanup-block for the closest enclosing try-with-finally or synchronized-expression.
     *
     *  `cleanups` is used:
     *
     *    (1) upon visiting a Return statement.
     *        In case of pending cleanups, we can't just emit a RETURN instruction, but must instead:
     *          - store the result (if any) in `earlyReturnVar`, and
     *          - jump to the next pending cleanup.
     *        See `genReturn()`
     *
     *    (2) upon emitting a try-with-finally or a synchronized-expr,
     *        In these cases, the targets of the above jumps are emitted,
     *        provided an early exit was actually encountered somewhere in the protected clauses.
     *        See `genLoadTry()` and `genSynchronized()`
     *
     *  The code thus emitted for jumps and targets covers the early-return case.
     *  The case of abrupt (ie exceptional) termination is covered by exception handlers
     *  emitted for that purpose as described in `genLoadTry()` and `genSynchronized()`.
     */
    var cleanups: List[asm.Label] = Nil
    def registerCleanup(finCleanup: asm.Label): Unit = {
      if (finCleanup != null) { cleanups = finCleanup :: cleanups }
    }
    def unregisterCleanup(finCleanup: asm.Label): Unit = {
      if (finCleanup != null) {
        assert(cleanups.head eq finCleanup,
               s"Bad nesting of cleanup operations: $cleanups trying to unregister: $finCleanup")
        cleanups = cleanups.tail
      }
    }

    /* ---------------- local variables and params ---------------- */

    case class Local(tk: BType, name: String, idx: Int, isSynth: Boolean)

    /*
     * Bookkeeping for method-local vars and method-params.
     *
     * TODO: use fewer slots. local variable slots are never re-used in separate blocks.
     * In the following example, x and y could use the same slot.
     *   def foo() = {
     *     { val x = 1 }
     *     { val y = "a" }
     *   }
     */
    object locals {

      private val slots = mutable.AnyRefMap.empty[Symbol, Local] // (local-or-param-sym -> Local(BType, name, idx, isSynth))

      private var nxtIdx = -1 // next available index for local-var

      def reset(isStaticMethod: Boolean): Unit = {
        slots.clear()
        nxtIdx = if (isStaticMethod) 0 else 1
      }

      def contains(locSym: Symbol): Boolean = { slots.contains(locSym) }

      def apply(locSym: Symbol): Local = { slots.apply(locSym) }

      /* Make a fresh local variable, ensuring a unique name.
       * The invoker must make sure inner classes are tracked for the sym's tpe.
       */
      def makeLocal(tk: BType, name: String): Symbol = {
        val locSym = methSymbol.newVariable(cunit.freshTermName(name), NoPosition, Flags.SYNTHETIC) // setInfo tpe
        makeLocal(locSym, tk)
        locSym
      }

      def makeLocal(locSym: Symbol): Local = {
        makeLocal(locSym, symInfoTK(locSym))
      }

      def getOrMakeLocal(locSym: Symbol): Local = {
        // `getOrElse` below has the same effect as `getOrElseUpdate` because `makeLocal()` adds an entry to the `locals` map.
        slots.getOrElse(locSym, makeLocal(locSym))
      }

      private def makeLocal(sym: Symbol, tk: BType): Local = {
        assert(nxtIdx != -1, "not a valid start index")
        val loc = Local(tk, sym.javaSimpleName.toString, nxtIdx, sym.isSynthetic)
        val existing = slots.put(sym, loc)
        if (existing.isDefined)
          globalError(sym.pos, "attempt to create duplicate local var.")
        assert(tk.size > 0, "makeLocal called for a symbol whose type is Unit.")
        nxtIdx += tk.size
        loc
      }

      // not to be confused with `fieldStore` and `fieldLoad` which also take a symbol but a field-symbol.
      def store(locSym: Symbol): Unit = {
        val Local(tk, _, idx, _) = slots(locSym)
        bc.store(idx, tk)
      }

      def load(locSym: Symbol): Unit = {
        val Local(tk, _, idx, _) = slots(locSym)
        bc.load(idx, tk)
      }

    }

    /* ---------------- Part 2 of program points, ie Labels in the ASM world ---------------- */

    /*
     *  The semantics of try-with-finally and synchronized-expr require their cleanup code
     *  to be present in three forms in the emitted bytecode:
     *    (a) as normal-exit code, reached via fall-through from the last program point being protected,
     *    (b) as code reached upon early-return from an enclosed return statement.
     *        The only difference between (a) and (b) is their next program-point:
     *          the former must continue with fall-through while
     *          the latter must continue to the next early-return cleanup (if any, otherwise return from the method).
     *        Otherwise they are identical.
     *    (c) as exception-handler, reached via exceptional control flow,
     *        which rethrows the caught exception once it's done with the cleanup code.
     *
     *  A particular cleanup may in general contain LabelDefs. Care is needed when duplicating such jump-targets,
     *  so as to preserve agreement with the (also duplicated) jump-sources.
     *  This is achieved based on the bookkeeping provided by two maps:
     *    - `labelDefsAtOrUnder` lists all LabelDefs enclosed by a given Tree node (the key)
     *    - `labelDef` provides the LabelDef node whose symbol is used as key.
     *       As a sidenote, a related map is `jumpDest`: it has the same keys as `labelDef` but its values are asm.Labels not LabelDef nodes.
     *
     *  Details in `emitFinalizer()`, which is invoked from `genLoadTry()` and `genSynchronized()`.
     */
    var labelDefsAtOrUnder: scala.collection.Map[Tree, List[LabelDef]] = null
    var labelDef: scala.collection.Map[Symbol, LabelDef] = null// (LabelDef-sym -> LabelDef)

    // bookkeeping the scopes of non-synthetic local vars, to emit debug info (`emitVars`).
    var varsInScope: List[Tuple2[Symbol, asm.Label]] = null // (local-var-sym -> start-of-scope)

    // helpers around program-points.
    def lastInsn: asm.tree.AbstractInsnNode = mnode.instructions.getLast
    def currProgramPoint(): asm.Label = {
      lastInsn match {
        case labnode: asm.tree.LabelNode => labnode.getLabel
        case _ =>
          val pp = new asm.Label
          mnode visitLabel pp
          pp
      }
    }
    def markProgramPoint(lbl: asm.Label): Unit = {
      val skip = (lbl == null) || isAtProgramPoint(lbl)
      if (!skip) { mnode visitLabel lbl }
    }
    def isAtProgramPoint(lbl: asm.Label): Boolean = {
      (lastInsn match { case labnode: asm.tree.LabelNode => (labnode.getLabel == lbl); case _ => false } )
    }
    def lineNumber(tree: Tree): Unit = {
      if (!emitLines || !tree.pos.isDefined) return
      val nr = tree.pos.finalPosition.line
      if (nr != lastEmittedLineNr) {
        lastEmittedLineNr = nr
        lastInsn match {
          case lnn: asm.tree.LineNumberNode =>
            // overwrite previous landmark as no instructions have been emitted for it
            lnn.line = nr
          case _ =>
            mnode.visitLineNumber(nr, currProgramPoint())
        }
      }
    }

    // on entering a method
    def resetMethodBookkeeping(dd: DefDef): Unit = {
      locals.reset(isStaticMethod = methSymbol.isStaticMember)
      jumpDest = immutable.Map.empty[ /* LabelDef */ Symbol, asm.Label ]
      // populate labelDefsAtOrUnder
      val ldf = new LabelDefsFinder(dd.rhs)
      ldf(dd.rhs)
      labelDefsAtOrUnder = ldf.result
      labelDef = ldf.directResult.map(ld => (ld.symbol -> ld)).toMap
      // check previous invocation of genDefDef exited as many varsInScope as it entered.
      assert(varsInScope == null, "Unbalanced entering/exiting of GenBCode's genBlock().")
      // check previous invocation of genDefDef unregistered as many cleanups as it registered.
      assert(cleanups == Nil, "Previous invocation of genDefDef didn't unregister as many cleanups as it registered.")
      isModuleInitialized = false
      earlyReturnVar      = null
      shouldEmitCleanup   = false

      lastEmittedLineNr = -1
    }

    /* ---------------- top-down traversal invoking ASM Tree API along the way ---------------- */

    def gen(tree: Tree): Unit = {
      tree match {
        case EmptyTree => ()

        case _: ModuleDef => abort(s"Modules should have been eliminated by refchecks: $tree")

        case ValDef(mods, name, tpt, rhs) => () // fields are added in `genPlainClass()`, via `addClassFields()`

        case dd : DefDef =>
          val sym = dd.symbol
          if (needsStaticImplMethod(sym)) {
            if (sym.isMixinConstructor) {
              val statified = global.gen.mkStatic(dd, sym.name, _.cloneSymbol)
              genDefDef(statified)
            } else {
              val forwarderDefDef = {
                val dd1 = global.gen.mkStatic(deriveDefDef(dd)(_ => EmptyTree), traitSuperAccessorName(sym), _.cloneSymbol.withoutAnnotations)
                dd1.symbol.setFlag(Flags.ARTIFACT).resetFlag(Flags.OVERRIDE)
                val selfParam :: realParams = dd1.vparamss.head.map(_.symbol)
                deriveDefDef(dd1)(_ =>
                  atPos(dd1.pos)(
                    Apply(Select(global.gen.mkAttributedIdent(selfParam).setType(sym.owner.typeConstructor), dd.symbol),
                    realParams.map(global.gen.mkAttributedIdent)).updateAttachment(UseInvokeSpecial))
                )
              }
              genDefDef(forwarderDefDef)
              genDefDef(dd)
            }
          } else genDefDef(dd)

        case Template(_, _, body) => body foreach gen

        case _ => abort(s"Illegal tree in gen: $tree")
      }
    }

    /*
     * must-single-thread
     */
    def initJMethod(flags: Int, params: List[Symbol]): Unit = {

      val jgensig = getGenericSignature(methSymbol, claszSymbol)

      val (excs, others) = methSymbol.annotations partition (_.symbol == definitions.ThrowsClass)
      val thrownExceptions: List[String] = getExceptions(excs)

      val bytecodeName =
        if (isMethSymStaticCtor) CLASS_CONSTRUCTOR_NAME
        else jMethodName

      val mdesc = methodBTypeFromSymbol(methSymbol).descriptor
      mnode = cnode.visitMethod(
        flags,
        bytecodeName,
        mdesc,
        jgensig,
        mkArray(thrownExceptions)
      ).asInstanceOf[asm.tree.MethodNode]

      emitParamNames(mnode, params)
      emitAnnotations(mnode, others)
      emitParamAnnotations(mnode, params.map(_.annotations))

    } // end of method initJMethod


    def genDefDef(dd: DefDef): Unit = {
      // the only method whose implementation is not emitted: getClass()
      if (definitions.isGetClass(dd.symbol)) { return }
      assert(mnode == null, "GenBCode detected nested method.")

      methSymbol  = dd.symbol
      jMethodName = methSymbol.javaSimpleName.toString
      returnType  = methodBTypeFromSymbol(dd.symbol).returnType
      isMethSymStaticCtor = methSymbol.isStaticConstructor

      resetMethodBookkeeping(dd)

      // add method-local vars for params
      val DefDef(_, _, _, vparamss, _, rhs) = dd
      assert(vparamss.isEmpty || vparamss.tail.isEmpty, s"Malformed parameter list: $vparamss")
      val params = if (vparamss.isEmpty) Nil else vparamss.head
      for (p <- params) { locals.makeLocal(p.symbol) }
      // debug assert((params.map(p => locals(p.symbol).tk)) == asmMethodType(methSymbol).getArgumentTypes.toList, "debug")

      if (params.size > MaximumJvmParameters) {
        // scala/bug#7324
        reporter.error(methSymbol.pos, s"Platform restriction: a parameter list's length cannot exceed $MaximumJvmParameters.")
        return
      }

      val isNative         = methSymbol.hasAnnotation(definitions.NativeAttr)
      val isAbstractMethod = rhs == EmptyTree
      val flags =
        javaFlags(methSymbol) |
        (if (isAbstractMethod)        asm.Opcodes.ACC_ABSTRACT   else 0) |
        (if (methSymbol.isStrictFP)   asm.Opcodes.ACC_STRICT     else 0) |
        (if (isNative)                asm.Opcodes.ACC_NATIVE     else 0)  // native methods of objects are generated in mirror classes


      initJMethod(flags, params.map(_.symbol))

      /* Add method-local vars for LabelDef-params.
       *
       * This makes sure that:
       *   (1) upon visiting any "forward-jumping" Apply (ie visited before its target LabelDef), and after
       *   (2) grabbing the corresponding param symbols,
       * those param-symbols can be used to access method-local vars.
       *
       * When duplicating a finally-contained LabelDef, another program-point is needed for the copy (each such copy has its own asm.Label),
       * but the same vars (given by the LabelDef's params) can be reused,
       * because no LabelDef ends up nested within itself after such duplication.
       */
      for(ld <- labelDefsAtOrUnder.getOrElse(dd.rhs, Nil); ldp <- ld.params; if !locals.contains(ldp.symbol)) {
        // the tail-calls xform results in symbols shared btw method-params and labelDef-params, thus the guard above.
        locals.makeLocal(ldp.symbol)
      }

      if (!isAbstractMethod && !isNative) {

        def emitNormalMethodBody(): Unit = {
          val veryFirstProgramPoint = currProgramPoint()
          genLoad(rhs, returnType)

          rhs match {
            case Return(_) | Block(_, Return(_)) | Throw(_) | Block(_, Throw(_)) => ()
            case EmptyTree =>
              globalError("Concrete method has no definition: " + dd + (
                if (settings.debug) "(found: " + methSymbol.owner.info.decls.toList.mkString(", ") + ")"
                else ""))
            case _ =>
              bc emitRETURN returnType
          }
          if (emitVars) {
            // add entries to LocalVariableTable JVM attribute
            val onePastLastProgramPoint = currProgramPoint()
            val hasStaticBitSet = ((flags & asm.Opcodes.ACC_STATIC) != 0)
            if (!hasStaticBitSet) {
              mnode.visitLocalVariable(
                "this",
                thisBType.descriptor,
                null,
                veryFirstProgramPoint,
                onePastLastProgramPoint,
                0
              )
            }
            for (p <- params) { emitLocalVarScope(p.symbol, veryFirstProgramPoint, onePastLastProgramPoint, force = true) }
          }

          if (isMethSymStaticCtor) { appendToStaticCtor(dd) }
        } // end of emitNormalMethodBody()

        lineNumber(rhs)
        emitNormalMethodBody()

        // Note we don't invoke visitMax, thus there are no FrameNode among mnode.instructions.
        // The only non-instruction nodes to be found are LabelNode and LineNumberNode.
      }

      if (AsmUtils.traceMethodEnabled && mnode.name.contains(AsmUtils.traceMethodPattern))
        AsmUtils.traceMethod(mnode)

      mnode = null
    } // end of method genDefDef()

    /*
     *  must-single-thread
     *
     *  TODO document, explain interplay with `fabricateStaticInit()`
     */
    private def appendToStaticCtor(dd: DefDef): Unit = {

      def insertBefore(
            location: asm.tree.AbstractInsnNode,
            i0: asm.tree.AbstractInsnNode,
            i1: asm.tree.AbstractInsnNode): Unit = {
        if (i0 != null) {
          mnode.instructions.insertBefore(location, i0.clone(null))
          mnode.instructions.insertBefore(location, i1.clone(null))
        }
      }

      // collect all return instructions
      var rets: List[asm.tree.AbstractInsnNode] = Nil
      mnode foreachInsn { i => if (i.getOpcode() == asm.Opcodes.RETURN) { rets ::= i  } }
      if (rets.isEmpty) { return }

      var insnModA: asm.tree.AbstractInsnNode = null
      var insnModB: asm.tree.AbstractInsnNode = null
      // call object's private ctor from static ctor
      if (isCZStaticModule) {
        // NEW `moduleName`
        val className = internalName(methSymbol.enclClass)
        insnModA      = new asm.tree.TypeInsnNode(asm.Opcodes.NEW, className)
        // INVOKESPECIAL <init>
        val callee = methSymbol.enclClass.primaryConstructor
        val jname  = callee.javaSimpleName.toString
        val jowner = internalName(callee.owner)
        val jtype  = methodBTypeFromSymbol(callee).descriptor
        insnModB   = new asm.tree.MethodInsnNode(asm.Opcodes.INVOKESPECIAL, jowner, jname, jtype, false)
      }

      var insnParcA: asm.tree.AbstractInsnNode = null
      var insnParcB: asm.tree.AbstractInsnNode = null
      // android creator code
      if (isCZParcelable) {
        // add a static field ("CREATOR") to this class to cache android.os.Parcelable$Creator
        val andrFieldDescr = classBTypeFromSymbol(AndroidCreatorClass).descriptor
        cnode.visitField(
          asm.Opcodes.ACC_STATIC | asm.Opcodes.ACC_FINAL,
          "CREATOR",
          andrFieldDescr,
          null,
          null
        )
        // INVOKESTATIC CREATOR(): android.os.Parcelable$Creator; -- TODO where does this Android method come from?
        val callee = definitions.getMember(claszSymbol.companionModule, androidFieldName)
        val jowner = internalName(callee.owner)
        val jname  = callee.javaSimpleName.toString
        val jtype  = methodBTypeFromSymbol(callee).descriptor
        insnParcA  = new asm.tree.MethodInsnNode(asm.Opcodes.INVOKESTATIC, jowner, jname, jtype, false)
        // PUTSTATIC `thisBType.internalName`.CREATOR;
        insnParcB  = new asm.tree.FieldInsnNode(asm.Opcodes.PUTSTATIC, thisBType.internalName, "CREATOR", andrFieldDescr)
      }

      // insert a few instructions for initialization before each return instruction
      for(r <- rets) {
        insertBefore(r, insnModA,  insnModB)
        insertBefore(r, insnParcA, insnParcB)
      }

    }

    def emitLocalVarScope(sym: Symbol, start: asm.Label, end: asm.Label, force: Boolean = false): Unit = {
      val Local(tk, name, idx, isSynth) = locals(sym)
      if (force || !isSynth) {
        mnode.visitLocalVariable(name, tk.descriptor, null, start, end, idx)
      }
    }

    def genLoad(tree: Tree, expectedType: BType): Unit

  } // end of class PlainSkelBuilder

}
