package scala.tools.nsc
package transform

import scala.collection.{ mutable, immutable }
import scala.reflect.internal.Flags

abstract class LazyVals extends Transform with TypingTransformers with ast.TreeDSL {
  // inherits abstract value `global` and class `Phase` from Transform

  import global._                  // the global environment
  import definitions._             // standard classes and methods
  import typer.{typed, atOwner}    // methods to type trees
  import CODE._

  val phaseName: String = "lazyvals"
  private val FLAGS_PER_BYTE: Int = 8 // Byte
  private def bitmapKindLocal = ByteClass

  def newTransformer(unit: CompilationUnit): Transformer =
    new LazyValues(unit)

  private def lazyUnit(sym: Symbol) = sym.tpe.resultType.typeSymbol == UnitClass

  object LocalLazyValFinder extends Traverser {
    var result: Boolean  = _

    def find(t: Tree) = {result = false; traverse(t); result}
    def find(ts: List[Tree]) = {result = false; traverseTrees(ts); result}

    override def traverse(t: Tree) {
      if (!result)
        t match {
          case v@ValDef(_, _, _, _) if v.symbol.isLazy =>
            result = true

          case d@DefDef(_, _, _, _, _, _) if d.symbol.isLazy && lazyUnit(d.symbol) =>
            d.symbol.resetFlag(symtab.Flags.LAZY)
            result = true

          case ClassDef(_, _, _, _) | DefDef(_, _, _, _, _, _) | ModuleDef(_, _, _) =>

          case LabelDef(name, _, _) if nme.isLoopHeaderLabel(name) =>

          case _ =>
            super.traverse(t)
        }
    }
  }

  private def isFieldWithBitmap(field: Symbol) = {
    field.info // ensure that nested objects are transformed
    // For checkinit consider normal value getters
    // but for lazy values only take into account lazy getters
    field.isLazy && field.isMethod && !field.isDeferred
  }

  /** Does this field require an initialized bit?
    *  Note: fields of classes inheriting DelayedInit are not checked.
    *        This is because they are neither initialized in the constructor
    *        nor do they have a setter (not if they are vals anyway). The usual
    *        logic for setting bitmaps does therefore not work for such fields.
    *        That's why they are excluded.
    *  Note: The `checkinit` option does not check if transient fields are initialized.
    */
  private def needsInitFlag(sym: Symbol) = (
    settings.checkInit
      && sym.isGetter
      && !sym.isInitializedToDefault
      && !isConstantType(sym.info.finalResultType) // SI-4742
      && !sym.hasFlag(Flags.PARAMACCESSOR | Flags.SPECIALIZED | Flags.LAZY)
      && !sym.accessed.hasFlag(Flags.PRESUPER)
      && !sym.isOuterAccessor
      && !(sym.owner isSubClass DelayedInitClass)
      && !(sym.accessed hasAnnotation TransientAttr)
    )

  /** Return a map of single-use fields to the lazy value that uses them during initialization.
    *  Each field has to be private and defined in the enclosing class, and there must
    *  be exactly one lazy value using it.
    *
    *  Such fields will be nulled after the initializer has memoized the lazy value.
    */
  def singleUseFields(templ: Template): scala.collection.Map[Symbol, List[Symbol]] = {
    val usedIn = mutable.HashMap[Symbol, List[Symbol]]() withDefaultValue Nil

    object SingleUseTraverser extends Traverser {
      override def traverse(tree: Tree) {
        tree match {
          case Assign(lhs, rhs) => traverse(rhs) // assignments don't count
          case _ =>
            if (tree.hasSymbolField && tree.symbol != NoSymbol) {
              val sym = tree.symbol
              if ((sym.hasAccessorFlag || (sym.isTerm && !sym.isMethod))
                && sym.isPrivate
                && !(currentOwner.isGetter && currentOwner.accessed == sym) // getter
                && !definitions.isPrimitiveValueClass(sym.tpe.resultType.typeSymbol)
                && sym.owner == templ.symbol.owner
                && !sym.isLazy
                && !tree.isDef) {
                debuglog("added use in: " + currentOwner + " -- " + tree)
                usedIn(sym) ::= currentOwner

              }
            }
            super.traverse(tree)
        }
      }
    }
    SingleUseTraverser(templ)
    debuglog("usedIn: " + usedIn)
    usedIn filter {
      case (_, member :: Nil) => member.isValue && member.isLazy
      case _                  => false
    }
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


  /**
   * Transform local lazy accessors to check for the initialized bit.
   */
  class LazyValues(unit: CompilationUnit) extends TypingTransformer(unit) {
    import symtab.Flags._

    /** map from method symbols to the number of lazy values it defines. */
    private val lazyVals = perRunCaches.newMap[Symbol, Int]() withDefaultValue 0

    /** Map a field symbol to a unique integer denoting its position in the class layout.
      *  For each class, fields defined by the class come after inherited fields. Mixed-in
      *  fields count as fields defined by the class itself.
      */
    private val fieldOffset = perRunCaches.newMap[Symbol, Int]()

    /** Map lazy values to the fields they should null after initialization. */
    private var lazyValNullables: Map[Symbol, Set[Symbol]] = _

    def findLazyValNullables(templ: Template): Unit = {
      lazyValNullables = nullableFields(templ) withDefaultValue Set()
    }

    private val bitmapKindForCategory = perRunCaches.newMap[Name, ClassSymbol]()

    // ByteClass, IntClass, LongClass
    private def bitmapKind(field: Symbol): ClassSymbol = bitmapKindForCategory(bitmapCategory(field))

    private def flagsPerBitmap(field: Symbol): Int = bitmapKind(field) match {
      case BooleanClass => 1
      case ByteClass    => 8
      case IntClass     => 32
      case LongClass    => 64
    }
    private def needsInitAndHasOffset(sym: Symbol) =
      needsInitFlag(sym) && (fieldOffset contains sym)

    /** Examines the symbol and returns a name indicating what brand of
      *  bitmap it requires.  The possibilities are the BITMAP_* vals
      *  defined in StdNames.  If it needs no bitmap, nme.NO_NAME.
      */
    private def bitmapCategory(field: Symbol): Name = {
      import nme._
      val isNormal = (
        if (isFieldWithBitmap(field)) true
        // bitmaps for checkinit fields are not inherited
        else if (needsInitFlag(field) && !field.isDeferred) false
        else return NO_NAME
        )
      if (field.accessed hasAnnotation TransientAttr) {
        if (isNormal) BITMAP_TRANSIENT
        else BITMAP_CHECKINIT_TRANSIENT
      } else {
        if (isNormal) BITMAP_NORMAL
        else BITMAP_CHECKINIT
      }
    }
    class AddNewDefs(clazz: Symbol, stats: List[Tree], localTyper: analyzer.Typer, addDef: (Position, Tree) => Unit) {
      buildBitmapOffsets()

      private def position(sym: Symbol) = if (sym.pos == NoPosition) clazz.pos else sym.pos

      /*
       *  Return the bitmap field for 'offset'. Depending on the hierarchy it is possible to reuse
       *  the bitmap of its parents. If that does not exist yet we create one.
       */
      private def bitmapFor(clazz0: Symbol, offset: Int, field: Symbol): Symbol = {
        val category   = bitmapCategory(field)
        val bitmapName = nme.newBitmapName(category, offset / flagsPerBitmap(field)).toTermName
        val sym        = clazz0.info.decl(bitmapName)

        assert(!sym.isOverloaded, sym)

        def createBitmap: Symbol = {
          val bitmapKind =  bitmapKindForCategory(category)
          val sym = clazz0.newVariable(bitmapName, clazz0.pos) setInfo bitmapKind.tpe
          enteringTyper(sym addAnnotation VolatileAttr)

          category match {
            case nme.BITMAP_TRANSIENT | nme.BITMAP_CHECKINIT_TRANSIENT => sym addAnnotation TransientAttr
            case _                                                     =>
          }
          val init = bitmapKind match {
            case BooleanClass => ValDef(sym, FALSE)
            case _            => ValDef(sym, ZERO)
          }

          sym setFlag Flags.PrivateLocal
          clazz0.info.decls.enter(sym)
          addDef(clazz0.pos, init)
          sym
        }

        sym orElse createBitmap
      }

      private def maskForOffset(offset: Int, sym: Symbol, kind: ClassSymbol): Tree = {
        def realOffset = offset % flagsPerBitmap(sym)
        if (kind == LongClass ) LIT(1L << realOffset) else LIT(1 << realOffset)
      }

      /* Return an (untyped) tree of the form 'Clazz.this.bmp = Clazz.this.bmp | mask'. */
      private def mkSetFlag(clazz: Symbol, offset: Int, valSym: Symbol, kind: ClassSymbol): Tree = {
        val bmp      = bitmapFor(clazz, offset, valSym)
        def mask     = maskForOffset(offset, valSym, kind)
        def x        = This(clazz) DOT bmp
        def newValue = if (kind == BooleanClass) TRUE else (x GEN_| (mask, kind))

        x === newValue
      }

      // A trait with a field of type Unit creates a trait setter (invoked by the
      // implementation class constructor), like for any other trait field.
      // However, no actual field is created in the class that mixes in the trait.
      // Therefore the setter does nothing (except setting the -Xcheckinit flag).
      def setInitFlag(getter: Symbol): List[Tree] =
        if (!needsInitFlag(getter)) Nil
        else List(mkSetFlag(clazz, fieldOffset(getter), getter, bitmapKind(getter)))

      /* Return an (untyped) tree of the form 'clazz.this.bitmapSym & mask (==|!=) 0', the
       * precise comparison operator depending on the value of 'equalToZero'.
       */
      private def mkTest(clazz: Symbol, mask: Tree, bitmapSym: Symbol, equalToZero: Boolean, kind: ClassSymbol): Tree = {
        val bitmapTree  = (This(clazz) DOT bitmapSym)
        def lhs         = bitmapTree GEN_& (mask, kind)
        kind match {
          case BooleanClass =>
            if (equalToZero)  NOT(bitmapTree)
            else              bitmapTree
          case _            =>
            if (equalToZero)  lhs GEN_== (ZERO, kind)
            else              lhs GEN_!= (ZERO, kind)
        }
      }

      private def mkSlowPathDef(clazz: Symbol, lzyVal: Symbol, cond: Tree, syncBody: List[Tree],
                        stats: List[Tree], retVal: Tree, attrThis: Tree, args: List[Tree]): Symbol = {
        val defSym = clazz.newMethod(nme.newLazyValSlowComputeName(lzyVal.name.toTermName), lzyVal.pos, PRIVATE)
        val params = defSym newSyntheticValueParams args.map(_.symbol.tpe)
        defSym setInfoAndEnter MethodType(params, lzyVal.tpe.resultType)
        val rhs: Tree = (gen.mkSynchronizedCheck(attrThis, cond, syncBody, stats)).changeOwner(currentOwner -> defSym)
        val strictSubst = new TreeSymSubstituterWithCopying(args.map(_.symbol), params)
        addDef(position(defSym), DefDef(defSym, strictSubst(BLOCK(rhs, retVal))))
        defSym
      }

      private def mkFastPathLazyBody(clazz: Symbol, lzyVal: Symbol, cond: Tree, syncBody: List[Tree],
                             stats: List[Tree], retVal: Tree): Tree = {
        mkFastPathBody(clazz, lzyVal, cond, syncBody, stats, retVal, gen.mkAttributedThis(clazz), List())
      }

      private def mkFastPathBody(clazz: Symbol, lzyVal: Symbol, cond: Tree, syncBody: List[Tree],
                         stats: List[Tree], retVal: Tree, attrThis: Tree, args: List[Tree]): Tree = {
        val slowPathSym: Symbol = mkSlowPathDef(clazz, lzyVal, cond, syncBody, stats, retVal, attrThis, args)
        If(cond, fn (This(clazz), slowPathSym, args.map(arg => Ident(arg.symbol)): _*), retVal)
      }


      /* Always copy the tree if we are going to perform sym substitution,
       * otherwise we will side-effect on the tree that is used in the fast path
       */
      private class TreeSymSubstituterWithCopying(from: List[Symbol], to: List[Symbol]) extends TreeSymSubstituter(from, to) {
        override def transform(tree: Tree): Tree = super.transform(tree.duplicate)

        override def apply[T <: Tree](tree: T): T = if (from.isEmpty) tree else super.apply(tree)
      }

      /*  return a 'lazified' version of rhs. It uses double-checked locking to ensure
       *  initialization is performed at most once. For performance reasons the double-checked
       *  locking is split into two parts, the first (fast) path checks the bitmap without
       *  synchronizing, and if that fails it initializes the lazy val within the
       *  synchronization block (slow path). This way the inliner should optimize
       *  the fast path because the method body is small enough.
       *  Private fields used only in this initializer are subsequently set to null.
       *
       *  @param clazz The class symbol
       *  @param lzyVal The symbol of this lazy field
       *  @param init The tree which initializes the field ( f = <rhs> )
       *  @param offset The offset of this field in the flags bitmap
       *
       *  The result will be a tree of the form
       *  { if ((bitmap&n & MASK) == 0) this.l$compute()
       *    else l$
       *
       *    ...
       *    def l$compute() = { synchronized(this) {
       *      if ((bitmap$n & MASK) == 0) {
       *        init // l$ = <rhs>
       *        bitmap$n = bimap$n | MASK
       *      }}
       *      l$
       *    }
       *
       *    ...
       *    this.f1 = null
       *    ... this.fn = null
       *  }
       *  where bitmap$n is a byte, int or long value acting as a bitmap of initialized values.
       *  The kind of the bitmap determines how many bit indicators for lazy vals are stored in it.
       *  For Int bitmap it is 32 and then 'n' in the above code is: (offset / 32),
       *  the MASK is (1 << (offset % 32)).
       *  If the class contains only a single lazy val then the bitmap is represented
       *  as a Boolean and the condition checking is a simple bool test.
       */
      private def mkLazyDef(clazz: Symbol, lzyVal: Symbol, init: List[Tree], retVal: Tree, offset: Int): Tree = {
        def nullify(sym: Symbol) = Select(This(clazz), sym.accessedOrSelf) === LIT(null)

        val bitmapSym = bitmapFor(clazz, offset, lzyVal)
        val kind      = bitmapKind(lzyVal)
        val mask      = maskForOffset(offset, lzyVal, kind)
        def cond      = mkTest(clazz, mask, bitmapSym, equalToZero = true, kind)
        val nulls     = lazyValNullables(lzyVal).toList sortBy (_.id) map nullify
        def syncBody  = init ::: List(mkSetFlag(clazz, offset, lzyVal, kind), UNIT)

        if (nulls.nonEmpty)
          log("nulling fields inside " + lzyVal + ": " + nulls)

        localTyper.typedPos(init.head.pos)(mkFastPathLazyBody(clazz, lzyVal, cond, syncBody, nulls, retVal))
      }

      def mkLazyDefGetter(lzyVal: Symbol, init: Tree, retVal: Tree): Tree =
        mkLazyDef(clazz, lzyVal, List(init), retVal, fieldOffset(lzyVal))

      def mkCheckedAccessorGetter(getter: Symbol, retVal: Tree): Tree = {
        if (!needsInitFlag(getter)) retVal
        else mkCheckedAccessor(clazz, retVal, fieldOffset(getter), getter.pos, getter)
      }

      private def mkCheckedAccessor(clazz: Symbol, retVal: Tree, offset: Int, pos: Position, fieldSym: Symbol): Tree = {
        val sym = fieldSym.getterIn(fieldSym.owner)
        val bitmapSym = bitmapFor(clazz, offset, sym)
        val kind      = bitmapKind(sym)
        val mask      = maskForOffset(offset, sym, kind)
        val msg       = s"Uninitialized field: ${unit.source}: ${pos.line}"
        val result    =
          IF (mkTest(clazz, mask, bitmapSym, equalToZero = false, kind)) .
            THEN (retVal) .
            ELSE (Throw(NewFromConstructor(UninitializedFieldConstructor, LIT(msg))))

        localTyper.typedPos(pos)(BLOCK(result, retVal))
      }

      /* Complete lazy field accessors. Applies only to classes,
       * for its own (non inherited) lazy fields. If 'checkinit'
       * is enabled, getters that check for the initialized bit are
       * generated, and the class constructor is changed to set the
       * initialized bits.
       */
      def addCheckedGetters(clazz: Symbol, stats: List[Tree]): List[Tree] = {
        def dd(stat: DefDef) = {
          val sym     = stat.symbol
          def isUnit  = sym.tpe.resultType.typeSymbol == UnitClass
          def isEmpty = stat.rhs == EmptyTree

          if (sym.isLazy && !isEmpty && !clazz.isImplClass) {
            assert(fieldOffset contains sym, sym)
            deriveDefDef(stat) {
              case t if isUnit => mkLazyDef(clazz, sym, List(t), UNIT, fieldOffset(sym))

              case Block(stats, res) =>
                mkLazyDef(clazz, sym, stats, Select(This(clazz), res.symbol), fieldOffset(sym))

              case t => t // pass specialized lazy vals through
            }
          }
          else if (needsInitFlag(sym) && !isEmpty && !clazz.hasFlag(Flags.IMPLCLASS | Flags.TRAIT)) {
            assert(fieldOffset contains sym, sym)
            deriveDefDef(stat)(rhs =>
              (mkCheckedAccessor(clazz, _: Tree, fieldOffset(sym), stat.pos, sym))(
                if (sym.tpe.resultType.typeSymbol == UnitClass) UNIT
                else rhs
              )
            )
          }
          else if (sym.isConstructor) {
            deriveDefDef(stat)(addInitBits(clazz, _))
          }
          else if (settings.checkInit && !clazz.isTrait && sym.isSetter) {
            val getter = sym.getterIn(clazz)
            if (needsInitFlag(getter) && fieldOffset.isDefinedAt(getter))
              deriveDefDef(stat)(rhs => Block(List(rhs, localTyper.typed(mkSetFlag(clazz, fieldOffset(getter), getter, bitmapKind(getter)))), UNIT))
            else stat
          }
          else stat
        }
        stats map {
          case defn: DefDef => dd(defn)
          case stat         => stat
        }
      }

      private class AddInitBitsTransformer(clazz: Symbol) extends Transformer {
        private def checkedGetter(lhs: Tree) = {
          val sym = clazz.info decl lhs.symbol.getterName suchThat (_.isGetter)
          if (needsInitAndHasOffset(sym)) {
            debuglog("adding checked getter for: " + sym + " " + lhs.symbol.flagString)
            List(localTyper typed mkSetFlag(clazz, fieldOffset(sym), sym, bitmapKind(sym)))
          }
          else Nil
        }
        override def transformStats(stats: List[Tree], exprOwner: Symbol) = {
          // !!! Ident(self) is never referenced, is it supposed to be confirming
          // that self is anything in particular?
          super.transformStats(
            stats flatMap {
              case stat @ Assign(lhs @ Select(This(_), _), rhs) => stat :: checkedGetter(lhs)
              // remove initialization for default values
              case Apply(lhs @ Select(Ident(self), _), EmptyTree.asList) if lhs.symbol.isSetter => Nil
              case stat => List(stat)
            },
            exprOwner
          )
        }
      }

      /* Adds statements to set the 'init' bit for each field initialized
       * in the body of a constructor.
       */
      private def addInitBits(clazz: Symbol, rhs: Tree): Tree =
        new AddInitBitsTransformer(clazz) transform rhs

      /* Fill the map from fields to offset numbers.
       * Instead of field symbols, the map keeps their getter symbols. This makes
       * code generation easier later.
       */
      private def buildBitmapOffsets() {
        def fold(fields: List[Symbol], category: Name) = {
          var idx = 0
          fields foreach { f =>
            fieldOffset(f) = idx
            idx += 1
          }

          if (idx == 0) ()
          else if (idx == 1) bitmapKindForCategory(category) = BooleanClass
          else if (idx < 9)  bitmapKindForCategory(category) = ByteClass
          else if (idx < 33) bitmapKindForCategory(category) = IntClass
          else bitmapKindForCategory(category)               = LongClass
        }
        clazz.info.decls.toList groupBy bitmapCategory foreach {
          case (nme.NO_NAME, _)            => ()
          case (category, fields)          => fold(fields, category)
        }
      }
    }

    private def flattenThickets(stats: List[Tree]): List[Tree] = stats.flatMap(_ match {
      case b @ Block(List(d1@DefDef(_, n1, _, _, _, _)), d2@DefDef(_, n2, _, _, _, _)) if b.tpe == null && n1.endsWith(nme.LAZY_SLOW_SUFFIX) =>
        List(d1, d2)
      case stat =>
        List(stat)
    })

    /** Perform the following transformations:
     *  - for a lazy accessor inside a method, make it check the initialization bitmap
     *  - for all methods, add enough int vars to allow one flag per lazy local value
     *  - blocks in template bodies behave almost like methods. A single bitmaps section is
     *      added in the first block, for all lazy values defined in such blocks.
     *  - remove ACCESSOR flags: accessors in traits are not statically implemented,
     *    but moved to the host class. local lazy values should be statically implemented.
     */
    override def transform(tree: Tree): Tree = {
      val sym = tree.symbol
      curTree = tree

      tree match {

        case Block(_, _) =>
          val block1 = super.transform(tree)
          val Block(stats, expr) = block1
          treeCopy.Block(block1, flattenThickets(stats), expr)

        case DefDef(_, _, _, _, _, rhs) => atOwner(tree.symbol) {
          val (res, slowPathDef) = if (!sym.owner.isClass && sym.isLazy) {
            val enclosingClassOrDummyOrMethod = {
              val enclMethod = sym.enclMethod

              if (enclMethod != NoSymbol) {
                val enclClass = sym.enclClass
                if (enclClass != NoSymbol && enclMethod == enclClass.enclMethod)
                  enclClass
                else
                  enclMethod
              } else
                sym.owner
            }
            debuglog(s"determined enclosing class/dummy/method for lazy val as $enclosingClassOrDummyOrMethod given symbol $sym")
            val idx = lazyVals(enclosingClassOrDummyOrMethod)
            lazyVals(enclosingClassOrDummyOrMethod) = idx + 1
            val (rhs1, sDef) = mkLazyDef(enclosingClassOrDummyOrMethod, transform(rhs), idx, sym)
            sym.resetFlag((if (lazyUnit(sym)) 0 else LAZY) | ACCESSOR)
            (rhs1, sDef)
          } else if (!sym.owner.isTrait && sym.isModule && sym.isMethod) {
            rhs match {
              case b @ Block((assign @ Assign(lhs, rhs)) :: Nil, expr) =>
                val cond = Apply(Select(lhs, Object_eq), List(Literal(Constant(null))))
                val moduleDefs = mkFastPathBody(sym.owner.enclClass, lhs.symbol, cond, transform(assign) :: Nil, Nil, transform(expr))
                (localTyper.typedPos(tree.pos)(moduleDefs._1), localTyper.typedPos(tree.pos)(moduleDefs._2))
              case rhs =>
                global.reporter.error(tree.pos, "Unexpected tree on the RHS of a module accessor: " + rhs)
                (rhs, EmptyTree)
            }
          } else {
            (transform(rhs), EmptyTree)
          }

          val ddef1 = deriveDefDef(tree)(_ => if (LocalLazyValFinder.find(res)) typed(addBitmapDefs(sym, res)) else res)
          if (slowPathDef != EmptyTree) {
            // The contents of this block are flattened into the enclosing statement sequence, see flattenThickets
            // This is a poor man's version of dotty's Thicket: https://github.com/lampepfl/dotty/blob/d5280358d1/src/dotty/tools/dotc/ast/Trees.scala#L707
            Block(slowPathDef, ddef1)
          } else ddef1
        }

        case Template(_, _, body) => atOwner(currentOwner) {
          val body1 = super.transformTrees(body)
          var added = false
          val stats =
            for (stat <- body1) yield stat match {
              case Block(_, _) | Apply(_, _) | If(_, _, _) | Try(_, _, _) if !added =>
                // Avoid adding bitmaps when they are fully overshadowed by those
                // that are added inside loops
                if (LocalLazyValFinder.find(stat)) {
                  added = true
                  typed(addBitmapDefs(sym, stat))
                } else stat
              case ValDef(_, _, _, _) =>
                typed(deriveValDef(stat)(addBitmapDefs(stat.symbol, _)))
              case _ =>
                stat
            }
          val innerClassBitmaps = if (!added && currentOwner.isClass && bitmaps.contains(currentOwner)) {
              // add bitmap to inner class if necessary
                val toAdd0 = bitmaps(currentOwner).map(s => typed(ValDef(s, ZERO)))
                toAdd0.foreach(t => {
                    if (currentOwner.info.decl(t.symbol.name) == NoSymbol) {
                      t.symbol.setFlag(PROTECTED)
                      currentOwner.info.decls.enter(t.symbol)
                    }
                })
                toAdd0
            } else List()
          deriveTemplate(tree)(_ => innerClassBitmaps ++ flattenThickets(stats))
        }

        case ValDef(_, _, _, _) if !sym.owner.isModule && !sym.owner.isClass =>
          deriveValDef(tree) { rhs0 =>
            val rhs = transform(rhs0)
            if (LocalLazyValFinder.find(rhs)) typed(addBitmapDefs(sym, rhs)) else rhs
          }

        case l@LabelDef(name0, params0, ifp0@If(_, _, _)) if name0.startsWith(nme.WHILE_PREFIX) =>
          val ifp1 = super.transform(ifp0)
          val If(cond0, thenp0, elsep0) = ifp1

          if (LocalLazyValFinder.find(thenp0))
            deriveLabelDef(l)(_ => treeCopy.If(ifp1, cond0, typed(addBitmapDefs(sym.owner, thenp0)), elsep0))
          else
            l

        case l@LabelDef(name0, params0, block@Block(stats0, expr))
          if name0.startsWith(nme.WHILE_PREFIX) || name0.startsWith(nme.DO_WHILE_PREFIX) =>
          val stats1 = super.transformTrees(stats0)
          if (LocalLazyValFinder.find(stats1))
            deriveLabelDef(l)(_ => treeCopy.Block(block, typed(addBitmapDefs(sym.owner, stats1.head))::stats1.tail, expr))
          else
            l

        case _ => super.transform(tree)
      }
    }

    /** Add the bitmap definitions to the rhs of a method definition.
     *  If the rhs has been tail-call transformed, insert the bitmap
     *  definitions inside the top-level label definition, so that each
     *  iteration has the lazy values uninitialized. Otherwise add them
     *  at the very beginning of the method.
     */
    private def addBitmapDefs(methSym: Symbol, rhs: Tree): Tree = {
      def prependStats(stats: List[Tree], tree: Tree): Block = tree match {
        case Block(stats1, res) => Block(stats ::: stats1, res)
        case _ => Block(stats, tree)
      }

      val bmps = bitmaps(methSym) map (ValDef(_, ZERO))

      def isMatch(params: List[Ident]) = (params.tail corresponds methSym.tpe.params)(_.tpe == _.tpe)

      if (bmps.isEmpty) rhs else rhs match {
        case Block(assign, l @ LabelDef(name, params, _))
          if (name string_== "_" + methSym.name) && isMatch(params) =>
            Block(assign, deriveLabelDef(l)(rhs => typed(prependStats(bmps, rhs))))

        case _ => prependStats(bmps, rhs)
      }
    }

    def mkSlowPathDef(clazz: Symbol, lzyVal: Symbol, cond: Tree, syncBody: List[Tree],
                      stats: List[Tree], retVal: Tree): Tree = {
      // Q: is there a reason to first set owner to `clazz` (by using clazz.newMethod), and then
      // changing it to lzyVal.owner very soon after? Could we just do lzyVal.owner.newMethod?
      val defSym = clazz.newMethod(nme.newLazyValSlowComputeName(lzyVal.name.toTermName), lzyVal.pos, STABLE | PRIVATE)
      defSym setInfo MethodType(List(), lzyVal.tpe.resultType)
      defSym.owner = lzyVal.owner
      debuglog(s"crete slow compute path $defSym with owner ${defSym.owner} for lazy val $lzyVal")
      if (bitmaps.contains(lzyVal))
        bitmaps(lzyVal).map(_.owner = defSym)
      val rhs: Tree = gen.mkSynchronizedCheck(clazz, cond, syncBody, stats).changeOwner(currentOwner -> defSym)

      DefDef(defSym, addBitmapDefs(lzyVal, BLOCK(rhs, retVal)))
    }


    def mkFastPathBody(clazz: Symbol, lzyVal: Symbol, cond: Tree, syncBody: List[Tree],
                       stats: List[Tree], retVal: Tree): (Tree, Tree) = {
      val slowPathDef: Tree = mkSlowPathDef(clazz, lzyVal, cond, syncBody, stats, retVal)
      (If(cond, Apply(Ident(slowPathDef.symbol), Nil), retVal), slowPathDef)
    }

    /** return a 'lazified' version of rhs. Rhs should conform to the
     *  following schema:
     *  {
     *    l$ = <rhs>
     *    l$
     *  } or
     *  <rhs> when the lazy value has type Unit (for which there is no field
     *  to cache its value.
     *
     *  Similarly as for normal lazy val members (see Mixin), the result will be a tree of the form
     *  { if ((bitmap&n & MASK) == 0) this.l$compute()
     *    else l$
     *
     *    def l$compute() = { synchronized(enclosing_class_or_dummy) {
     *      if ((bitmap$n & MASK) == 0) {
     *       l$ = <rhs>
     *       bitmap$n = bimap$n | MASK
     *      }}
     *      l$
     *    }
     *  }
     *  where bitmap$n is a byte value acting as a bitmap of initialized values. It is
     *  the 'n' is (offset / 8), the MASK is (1 << (offset % 8)). If the value has type
     *  unit, no field is used to cache the value, so the l$compute will now look as following:
     *  {
     *    def l$compute() = { synchronized(enclosing_class_or_dummy) {
     *      if ((bitmap$n & MASK) == 0) {
     *       <rhs>;
     *       bitmap$n = bimap$n | MASK
     *      }}
     *    ()
     *    }
     *  }
     */
    private def mkLazyDef(methOrClass: Symbol, tree: Tree, offset: Int, lazyVal: Symbol): (Tree, Tree) = {
      val bitmapSym           = getBitmapFor(methOrClass, offset)
      val mask                = LIT(1 << (offset % FLAGS_PER_BYTE))
      val bitmapRef = if (methOrClass.isClass) Select(This(methOrClass), bitmapSym) else Ident(bitmapSym)

      def mkBlock(stmt: Tree) = BLOCK(stmt, mkSetFlag(bitmapSym, mask, bitmapRef), UNIT)

      debuglog(s"create complete lazy def in $methOrClass for $lazyVal")
      val (block, res) = tree match {
        case Block(List(assignment), res) if !lazyUnit(lazyVal) =>
          (mkBlock(assignment),  res)
        case rhs                          =>
          (mkBlock(rhs),         UNIT)
      }

      val cond = (bitmapRef GEN_& (mask, bitmapKindLocal)) GEN_== (ZERO, bitmapKindLocal)
      val lazyDefs = mkFastPathBody(methOrClass.enclClass, lazyVal, cond, List(block), Nil, res)
      (atPos(tree.pos)(localTyper.typed {lazyDefs._1 }), atPos(tree.pos)(localTyper.typed {lazyDefs._2 }))
    }

    private def mkSetFlag(bmp: Symbol, mask: Tree, bmpRef: Tree): Tree =
      bmpRef === (bmpRef GEN_| (mask, bitmapKindLocal))

    val bitmaps = mutable.Map[Symbol, List[Symbol]]() withDefaultValue Nil

    /** Return the symbol corresponding of the right bitmap int inside meth,
     *  given offset.
     */
    private def getBitmapFor(meth: Symbol, offset: Int): Symbol = {
      val n = offset / FLAGS_PER_BYTE
      val bmps = bitmaps(meth)
      if (bmps.length > n)
        bmps(n)
      else {
        val sym = meth.newVariable(nme.newBitmapName(nme.BITMAP_NORMAL, n), meth.pos).setInfo(ByteTpe)
        enteringTyper {
          sym addAnnotation VolatileAttr
        }

        bitmaps(meth) = (sym :: bmps).reverse
        sym
      }
    }
  }
}
