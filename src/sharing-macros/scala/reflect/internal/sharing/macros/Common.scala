package scala.reflect.internal.sharing.macros

import scala.reflect.macros.blackbox

/** What the macros of the Scala 2.13 build share. The expansions refer to the library by fully qualified names, and to
 *  its internal evidence objects by typed references, which the typer does not check for access again.
 */
private[macros] trait Common {
  val c: blackbox.Context

  import c.universe._
  import c.internal.{changeOwner, defDef, enclosingOwner, gen, methodType, newMethodSymbol, nullaryMethodType, newTermSymbol, setInfo, setOwner,
    substituteSymbols, valDef}

  final val pkg: Tree = q"_root_.scala.reflect.internal.sharing"
  final def publish: Tree = q"$pkg.Publish"

  private def module(name: String): ModuleSymbol = c.mirror.staticModule("scala.reflect.internal.sharing." + name)
  private lazy val SharingModule = module("Sharing")
  private lazy val PublishedModule = module("Published")
  private lazy val PackageObject = module("package")

  private def tycon(owner: ModuleSymbol, name: String): Type =
    owner.moduleClass.info.decl(TypeName(name)).asType.toTypeConstructor

  lazy val PublishedAlias: Symbol = PackageObject.moduleClass.info.decl(TypeName("Published"))
  lazy val OnceSym: Symbol = PackageObject.moduleClass.info.decl(TypeName("Once"))
  lazy val PayloadSym: Symbol = PackageObject.moduleClass.info.decl(TypeName("Payload"))
  def OnceType(k: Type, t: Type): Type = appliedType(tycon(PackageObject, "Once"), List(k, t))
  def PayloadType(o: Type, f: Type, t: Type): Type = appliedType(tycon(PackageObject, "Payload"), List(o, f, t))
  def ClaimedType(k: Type, t: Type): Type = appliedType(tycon(PackageObject, "Claimed"), List(k, t))
  def HoldsType(k: Type): Type = appliedType(tycon(SharingModule, "Holds"), List(k))
  def MonitorType(k: Type): Type = appliedType(tycon(SharingModule, "Monitor"), List(k))
  def HandleType(o: Type, f: Type): Type = appliedType(tycon(PublishedModule, "Handle"), List(o, f))
  def AcquiredType(s: Type, f: Type, t: Type): Type = appliedType(tycon(PublishedModule, "Acquired"), List(s, f, t))
  def PublishingType: Type = tycon(PublishedModule, "Publishing")

  /** The shared evidence object, `Sharing.held` or `Sharing.monitorHeld` (private to the library), as `tpe`. */
  def evidence(tpe: Type): Tree = {
    val name = if (tpe.typeSymbol.name == TypeName("Holds")) "held" else "monitorHeld"
    q"${gen.mkAttributedRef(SharingModule.moduleClass.info.decl(TermName(name)))}.asInstanceOf[$tpe]"
  }

  /** The receiver of an operation of an implicit class: `x` in `OnceOps[K, T](x).get(...)`. */
  def prefixArg: Tree = c.prefix.tree match {
    case Apply(_, List(arg)) => arg
    case t => c.abort(t.pos, s"an operation on an implicit class instance, not on ${t}")
  }

  def strip(t: Tree): Tree = t match {
    case Typed(e, _) => strip(e)
    case Block(Nil, e) => strip(e)
    case _ => t
  }

  def isNullLiteral(t: Tree): Boolean = strip(t) match {
    case Literal(Constant(null)) => true
    case _ => false
  }

  /** Whether `t` defines symbols, so that it cannot appear twice in an expansion, nor move to another owner. */
  def definesSymbols(t: Tree): Boolean = t.exists {
    case _: DefTree | _: Function => true
    case _ => false
  }

  /** A `var` field of a class, as the receiver of an operation: the tree, its qualifier (the owner) and its getter. */
  final class Field(val tree: Tree, val qual: Tree, val getter: MethodSymbol) {
    def name: String = getter.name.decodedName.toString
    def ownerClass: ClassSymbol = getter.owner.asClass
    /** The field's declared type, as seen from its qualifier, aliases kept. */
    def declared: Type = getter.typeSignatureIn(qual.tpe).finalResultType
    def read: Tree = tree.duplicate
    def owner: Tree = qual.duplicate
    def store(v: Tree): Tree = q"${tree.duplicate} = $v"
  }

  def classVar(t: Tree, what: String): Field = strip(t) match {
    case s @ Select(qual, _) if s.symbol != null && s.symbol.isMethod && s.symbol.asMethod.isGetter &&
        s.symbol.asMethod.setter != NoSymbol =>
      new Field(s, qual, s.symbol.asMethod)
    case other => c.abort(t.pos, s"$what needs a `var` field of a class, not $other")
  }

  /** The implicit `Published.Handle[Owner, "name"]` of a field. */
  def handleFor(f: Field): Tree = {
    val tpe = HandleType(f.ownerClass.toType, c.internal.constantType(Constant(f.name)))
    val h = c.inferImplicitValue(tpe, silent = true)
    if (h.isEmpty)
      c.abort(f.tree.pos, s"no implicit `$tpe`: declare `implicit val ${f.name}Handle: $tpe = Published.handle` in ${f.ownerClass.name}'s companion")
    h
  }

  /** A function literal's body as a local method, for code that more than one branch of an expansion runs: a Scala 2
   *  macro cannot copy a typed tree that defines symbols. The literal's parameter becomes the method's, and `wrap`
   *  builds the method's body from the literal's. Any other function value is bound to a val and applied.
   *
   *  @return the definitions to put first in the expansion, and a call of the method with an argument
   */
  def localOp(fun: Tree, resultType: Type, prefix: String, wrap: Tree => Tree = identity): (List[Tree], Tree => Tree) =
    strip(fun) match {
      case f @ Function(List(param), body) =>
        val m = newMethodSymbol(enclosingOwner, TermName(c.freshName(prefix)), f.pos, Flag.SYNTHETIC)
        setOwner(param.symbol, m)
        setInfo(m, methodType(List(param.symbol), resultType))
        val dd = defDef(m, wrap(changeOwner(body, f.symbol, m)))
        (List(dd), arg => q"${gen.mkAttributedIdent(m)}($arg)")
      case other =>
        val fn = TermName(c.freshName(prefix + "Fn"))
        val m = TermName(c.freshName(prefix))
        val p = TermName(c.freshName("ev"))
        val paramType = other.tpe.typeArgs.head
        (List(q"val $fn = $other", q"def $m($p: $paramType): $resultType = ${wrap(q"$fn.apply($p)")}"), arg => q"$m($arg)")
    }

  /** The evidence parameter's type of a function literal or value, `P` of its `P => T`. */
  def paramType(fun: Tree): Type = fun.tpe.typeArgs.head

  /** `fun(args)`, beta-reduced if `fun` is a literal: for code an expansion runs once, at its top level. */
  def inlineApply(fun: Tree, args: List[Tree]): Tree = strip(fun) match {
    case f @ Function(params, body) if params.length == args.length =>
      val owner = enclosingOwner
      val locals = params.map { p =>
        val s = newTermSymbol(owner, TermName(c.freshName(p.name.toString)), p.pos, NoFlags)
        setInfo(s, p.symbol.info)
      }
      val vals = locals.zip(args).map { case (s, a) => valDef(s, a) }
      val body1 = substituteSymbols(changeOwner(body, f.symbol, owner), params.map(_.symbol), locals)
      q"{ ..$vals; $body1 }"
    case other => q"$other.apply(..$args)"
  }

  /** A tree to evaluate in more than one place: copies of it, if it defines nothing; else calls of a local method. */
  def reusable(t: Tree, tpe: Type, prefix: String): (List[Tree], () => Tree) =
    if (!definesSymbols(t)) (Nil, () => t.duplicate)
    else {
      val m = newMethodSymbol(enclosingOwner, TermName(c.freshName(prefix)), t.pos, Flag.SYNTHETIC)
      setInfo(m, nullaryMethodType(tpe))
      (List(defDef(m, changeOwner(t, enclosingOwner, m))), () => gen.mkAttributedIdent(m))
    }

  /** The kind of value a published field holds, once erased: `Int`, `Long`, `Boolean` or `Ref`, and, for a value class
   *  over one of them, its class and the accessor of its one field. */
  final class Content(val tpe: Type, val kind: String, val valueClass: Option[(ClassSymbol, MethodSymbol)]) {
    def erased: Type = valueClass match {
      case Some((cls, acc)) => acc.typeSignatureIn(tpe).finalResultType
      case None => tpe
    }
    def wrap(u: Tree): Tree = valueClass match {
      case Some(_) => q"new $tpe($u)"
      case None if kind == "Ref" => q"$u.asInstanceOf[$tpe]"
      case None => u
    }
    // the accessor may be private: a typed selection is not checked for access again
    def unwrap(v: Tree): Tree = valueClass match {
      case Some((_, acc)) => gen.mkAttributedSelect(v, acc)
      case None => v
    }
  }

  def contentOf(t: Type, pos: Position): Content = {
    def kindOf(e: Type): String =
      if (e =:= typeOf[Int]) "Int"
      else if (e =:= typeOf[Long]) "Long"
      else if (e =:= typeOf[Boolean]) "Boolean"
      else if (e <:< typeOf[AnyRef]) "Ref"
      else c.abort(pos, s"a `Published` field of $e is not supported (Int, Long, Boolean, a reference)")
    val d = t.dealias
    val sym = d.typeSymbol
    if (sym.isClass && sym.asClass.isDerivedValueClass) {
      val acc = d.decls.collectFirst { case m: MethodSymbol if m.isParamAccessor => m }.get
      new Content(d, kindOf(acc.typeSignatureIn(d).finalResultType), Some((sym.asClass, acc)))
    } else new Content(d, kindOf(d), None)
  }
}
