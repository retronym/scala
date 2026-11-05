package io.github.retronym.sharing.macros

import scala.reflect.macros.{blackbox, whitebox}

private[macros] trait PublishedSupport extends Common {
  import c.universe._

  /** A `var` declared `Published[T]`, and what it holds. */
  def publishedField(t: Tree): (Field, Content) = {
    val f = classVar(t, "a `Published` operation")
    f.declared match {
      case TypeRef(_, sym, List(content)) if sym == PublishedAlias => (f, contentOf(content, t.pos))
      case other => c.abort(t.pos, s"a `Published` operation needs a `var` declared `Published[T]`, not ${f.tree}: $other")
    }
  }

  def acquireTree(f: Field, content: Content): Tree =
    content.wrap(q"$publish.${TermName("getAcquire" + content.kind)}(${handleFor(f)}, ${f.owner})")

  def releaseTree(f: Field, content: Content, v: Tree): Tree =
    q"$publish.${TermName("setRelease" + content.kind)}(${handleFor(f)}, ${f.owner}, ${content.unwrap(v)})"
}

/** The macros of `Published` and `Payload`. Each access of a published field is a static method of `Publish`, the
 *  `VarHandle` call, given the handle the companion of the field's class declares.
 */
class PublishedMacros(val c: blackbox.Context) extends PublishedSupport {
  import c.universe._

  def acquire[T: c.WeakTypeTag]: Tree = {
    val (f, content) = publishedField(prefixArg)
    acquireTree(f, content)
  }

  def release[T: c.WeakTypeTag](v: Tree): Tree = {
    val (f, content) = publishedField(prefixArg)
    releaseTree(f, content, v)
  }

  def plain[T: c.WeakTypeTag]: Tree = publishedField(prefixArg)._1.read

  def setPlain[T: c.WeakTypeTag](v: Tree): Tree = publishedField(prefixArg)._1.store(v)

  def acquiredValue[T: c.WeakTypeTag]: Tree = q"$prefixArg.asInstanceOf[${weakTypeOf[T]}]"

  def handle[O: c.WeakTypeTag, F: c.WeakTypeTag]: Tree = {
    val o = weakTypeOf[O]
    val name = weakTypeOf[F].dealias match {
      case ConstantType(Constant(n: String)) => n
      case t => c.abort(c.enclosingPosition, s"a field name as a literal type, not $t")
    }
    val cls = o.typeSymbol
    val getter = o.decl(TermName(name))
    if (getter == NoSymbol || !getter.isMethod || !getter.asMethod.isGetter)
      c.abort(c.enclosingPosition, s"${cls.name} has no field `$name`")
    if (getter.isPrivate)
      c.abort(c.enclosingPosition, s"${cls.name}.$name is private, and its field is reached only through its handle: make it private[${cls.owner.name}]")
    val erased: Type = getter.typeSignature.finalResultType match {
      case TypeRef(_, sym, List(content)) if sym == PublishedAlias => contentOf(content, c.enclosingPosition).erased.erasure
      case TypeRef(_, sym, _) if sym == OnceSym => typeOf[AnyRef]
      case other => c.abort(c.enclosingPosition, s"${cls.name}.$name is a $other, not a `Published` or a `Once`")
    }
    q"$publish.fieldHandle(${Literal(Constant(o.erasure))}, $name, ${Literal(Constant(erased))}).asInstanceOf[${HandleType(o, weakTypeOf[F])}]"
  }

  // ---- payloads

  /** A `Payload[O, F, T]` field: the field, and its publisher's name. */
  private def payloadField(t: Tree): (Field, String) = {
    val f = classVar(t, "a `Payload` operation")
    (f, publisherOf(f.declared).getOrElse(c.abort(t.pos, s"${f.tree} is not a `Payload`")))
  }

  private def publisherOf(t: Type): Option[String] = t match {
    case TypeRef(_, sym, List(_, f, _)) if sym == PayloadSym => f.dealias match {
      case ConstantType(Constant(n: String)) => Some(n)
      case _ => None
    }
    case _ => None
  }

  def after[S: c.WeakTypeTag, T: c.WeakTypeTag](snap: Tree): Tree = {
    val (f, _) = payloadField(prefixArg)
    if (!(weakTypeOf[S] =:= f.qual.tpe))
      c.abort(snap.pos, s"${f.tree} is read after the acquire of another object's publisher (${weakTypeOf[S]}, not ${f.qual.tpe})")
    q"${f.read}.asInstanceOf[${weakTypeOf[T]}]"
  }

  def payloadPlain[T: c.WeakTypeTag]: Tree = q"${payloadField(prefixArg)._1.read}.asInstanceOf[${weakTypeOf[T]}]"

  def payloadStore[O: c.WeakTypeTag, F: c.WeakTypeTag, T: c.WeakTypeTag](v: Tree)(p: Tree): Tree = {
    val (f, _) = payloadField(prefixArg)
    f.store(q"$v.asInstanceOf[${PayloadType(weakTypeOf[O], weakTypeOf[F], weakTypeOf[T])}]")
  }

  def publishing[T: c.WeakTypeTag](v: Tree)(body: Tree): Tree = {
    val (f, content) = publishedField(prefixArg)
    strip(body) match {
      case Function(List(evidence), rhs) =>
        // the stores in the block have been expanded already (`:=` drops its evidence), so any use of the evidence
        // left is one that a store did not consume: it would let a store outlive the block
        new Traverser {
          override def traverse(tree: Tree): Unit = tree match {
            case id: Ident if id.symbol == evidence.symbol =>
              c.error(id.pos, "the evidence of a `publishing` block may only be passed to `:=` in it")
            // a store of a payload, which the typer made a call of its setter: of this publisher, on this object
            case Apply(sel @ Select(qual, _), List(_)) if sel.symbol != null && sel.symbol.isMethod &&
                sel.symbol.asMethod.isSetter =>
              publisherOf(sel.symbol.asMethod.paramLists.head.head.typeSignature).foreach { pub =>
                if (pub != f.name || !(qual.tpe =:= f.qual.tpe))
                  c.error(sel.pos, s"$sel is not published by ${f.tree}, so it is not stored in its `publishing` block")
              }
              super.traverse(tree)
            case _ => super.traverse(tree)
          }
        }.traverse(rhs)
      case _ => c.abort(body.pos, "`publishing` takes a function literal, so that its stores can be checked")
    }
    val run = inlineApply(body, List(q"null.asInstanceOf[$PublishingType]"))
    q"{ $run; ${releaseTree(f, content, v)} }"
  }
}

/** `acquired`, whose type names the field's owner and name: a whitebox macro, as Scala 3's is a transparent inline. */
class PublishedWhiteboxMacros(val c: whitebox.Context) extends PublishedSupport {
  import c.universe._

  def acquired[T: c.WeakTypeTag]: Tree = {
    val (f, content) = publishedField(prefixArg)
    val tpe = AcquiredType(f.qual.tpe, c.internal.constantType(Constant(f.name)), content.tpe)
    q"${acquireTree(f, content)}.asInstanceOf[$tpe]"
  }
}
