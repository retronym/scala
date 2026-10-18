package scala.tools.nsc.compilerlab

/** A distillation of how a typing transformer tracks the owner, and the context for it, as it descends; one op
 *  transforms a tree of definitions and expressions, changing nothing. Two choices are crossed:
 *
 *  How a definition enters its owner (the protocol):
 *  - `closure`: a by-name `atOwner` block, as `Tree.transform` does today. Allocation-free only if the JIT inlines
 *    the whole `atOwner` chain into the caller and scalar-replaces the closure.
 *  - `closure-finally`: the same, the subclass restoring its state in a `finally`.
 *  - `push-pop`: `pushOwner` before the body, `popOwner` after; no closure.
 *  - `push-pop-finally`: the same with `popOwner` in a `finally`.
 *  - `legacy`: push-pop definitions, but the transformer overrides `atOwner`, as `SuperAccessors` does (to track
 *    `validCurrentOwner`). Such a transformer must still see every owner through its override, so definitions fall
 *    back to `atOwner` blocks for it: `Transformer.pushPopOwners`.
 *
 *  What a transformer keeps per owner (the contexts):
 *  - `plain`: only `currentOwner`, as a plain `Transformer`.
 *  - `eager`: a context made for each owner entered, as `TypingTransformer` before lazy contexts.
 *  - `lazy`: a frame pushed onto arrays, the context made only when read, as `TypingTransformer` after.
 *
 *  `OwnerProtocolToy <contexts>-<protocol> [readEvery] [seconds]`, one variant per JVM so that each call site sees one
 *  transformer class, as in a compile where one phase's transformer runs at a time. With `readEvery` = N the context is
 *  read at every N-th definition, as a phase that types the odd tree; 0 reads nothing.
 */
object OwnerProtocolToy {
  final class Sym(val id: Int)

  /** A typer context: one per owner, chained to its enclosing one. */
  final class Context(val outer: Context, val owner: Sym)

  abstract class Tree { def transform(t: Transformer): Tree }

  final class Leaf(val n: Int) extends Tree { def transform(t: Transformer): Tree = this }

  /** An expression: children, no owner. */
  final class Apply(val args: Array[Tree]) extends Tree {
    def transform(t: Transformer): Tree = {
      val args1 = t.transformTrees(args)
      if (args1 eq args) this else new Apply(args1)
    }
  }

  /** A definition, its body transformed with it as the owner, entered with an `atOwner` block. */
  final class DefnClosure(val sym: Sym, val body: Array[Tree]) extends Tree {
    def transform(t: Transformer): Tree = t.atOwner(sym) {
      t.visitDefinition()
      val body1 = t.transformTrees(body)
      if (body1 eq body) this else new DefnClosure(sym, body1)
    }
  }

  /** A definition entered with `pushOwner` / `popOwner`, or by `atOwner` for a transformer that overrides it. */
  final class DefnPushPop(val sym: Sym, val body: Array[Tree]) extends Tree {
    def transform(t: Transformer): Tree =
      if (t.pushPopOwners) {
        t.pushOwner(sym)
        val result = transformBody(t)
        t.popOwner()
        result
      } else t.atOwner(sym)(transformBody(t))
    private def transformBody(t: Transformer): Tree = {
      t.visitDefinition()
      val body1 = t.transformTrees(body)
      if (body1 eq body) this else new DefnPushPop(sym, body1)
    }
  }

  /** A definition entered with `pushOwner` and left with `popOwner` in a `finally`. */
  final class DefnPushPopFinally(val sym: Sym, val body: Array[Tree]) extends Tree {
    def transform(t: Transformer): Tree =
      if (t.pushPopOwners) {
        t.pushOwner(sym)
        try transformBody(t) finally t.popOwner()
      } else t.atOwner(sym)(transformBody(t))
    private def transformBody(t: Transformer): Tree = {
      t.visitDefinition()
      val body1 = t.transformTrees(body)
      if (body1 eq body) this else new DefnPushPopFinally(sym, body1)
    }
  }

  /** As `scala.reflect.api.Trees#Transformer`, with `pushOwner` / `popOwner` beside `atOwner`. */
  abstract class Transformer {
    /** May trees enter owners with `pushOwner` / `popOwner`? Not if this transformer overrides `atOwner` (other than in
     *  a class that keeps the two in step), which must then see every owner. */
    final val pushPopOwners: Boolean = Transformer.pushPopOwners.get(getClass)

    var currentOwner: Sym = null
    private[this] var savedOwners = new Array[Sym](16)
    private[this] var depth = 0

    def transform(tree: Tree): Tree = tree.transform(this)

    final def transformTrees(trees: Array[Tree]): Array[Tree] = {
      var i = 0
      var out: Array[Tree] = null
      while (i < trees.length) {
        val t = trees(i)
        val t1 = transform(t)
        if ((t1 ne t) && (out eq null)) out = trees.clone()
        if (out ne null) out(i) = t1
        i += 1
      }
      if (out eq null) trees else out
    }

    /** Hooks into owner changes are `pushOwner` / `popOwner`; overriding this one makes trees call it (with a closure)
     *  instead of those. */
    def atOwner[A](owner: Sym)(trans: => A): A = {
      pushOwner(owner)
      val result = trans
      popOwner()
      result
    }

    def pushOwner(owner: Sym): Unit = {
      if (depth == savedOwners.length) savedOwners = java.util.Arrays.copyOf(savedOwners, depth * 2)
      savedOwners(depth) = currentOwner
      depth += 1
      currentOwner = owner
    }

    def popOwner(): Unit = {
      depth -= 1
      currentOwner = savedOwners(depth)
      savedOwners(depth) = null
    }

    /** Called inside each definition, after entering its owner: a phase's work there. */
    def visitDefinition(): Unit = ()
  }

  object Transformer {
    /** Classes whose `atOwner` is `pushOwner`, the block, `popOwner`: an override there needs no closure path. */
    private val keepsInStep: Set[Class[_]] = Set(classOf[Transformer], classOf[PushPopTransformer])

    val pushPopOwners: ClassValue[java.lang.Boolean] = new ClassValue[java.lang.Boolean] {
      protected def computeValue(c: Class[_]): java.lang.Boolean = {
        // the nearest class declaring `atOwner`
        val declaring = Iterator.iterate[Class[_]](c)(_.getSuperclass).takeWhile(_ != null)
          .find(_.getDeclaredMethods.exists(_.getName == "atOwner")).get
        keepsInStep(declaring)
      }
    }
  }

  /** What a typing transformer keeps per owner: an `enter` / `exit` pair around each, and the context, read with
   *  `localContext`. */
  abstract class Contexts(readEvery: Int) {
    def enter(owner: Sym): Int
    def exit(i: Int): Unit
    def localContext: Context
    private[this] var definitions = 0
    var read: Context = null
    final def visitDefinition(): Unit =
      if (readEvery > 0) {
        definitions += 1
        if (definitions % readEvery == 0) read = localContext
      }
  }

  final class NoContexts extends Contexts(0) {
    def enter(owner: Sym): Int = 0
    def exit(i: Int): Unit = ()
    def localContext: Context = null
  }

  /** Before: a context made at each owner, the enclosing one restored when it exits. */
  final class EagerContexts(readEvery: Int) extends Contexts(readEvery) {
    private[this] var context = new Context(null, null)
    private[this] var saved = new Array[Context](16)
    private[this] var frames = 0
    def enter(owner: Sym): Int = {
      val i = frames
      if (i == saved.length) saved = java.util.Arrays.copyOf(saved, i * 2)
      saved(i) = context
      context = new Context(context, owner)
      frames = i + 1
      i
    }
    def exit(i: Int): Unit = {
      context = saved(i)
      saved(i) = null
      frames = i
    }
    def localContext: Context = context
  }

  /** After: a frame pushed at each owner; the contexts of the pending frames made when the context is read. */
  final class LazyContexts(readEvery: Int) extends Contexts(readEvery) {
    private[this] var context = new Context(null, null)
    private[this] var owners = new Array[Sym](16)
    private[this] var parents = new Array[Context](16)
    private[this] var frames = 0
    private[this] var materialized = 0
    def enter(owner: Sym): Int = {
      val i = frames
      if (i == owners.length) {
        owners = java.util.Arrays.copyOf(owners, i * 2)
        parents = java.util.Arrays.copyOf(parents, i * 2)
      }
      owners(i) = owner
      frames = i + 1
      i
    }
    def exit(i: Int): Unit = {
      if (materialized > i) {
        context = parents(i)
        parents(i) = null
        materialized = i
      }
      owners(i) = null
      frames = i
    }
    def localContext: Context = {
      while (materialized < frames) {
        val i = materialized
        parents(i) = context
        context = new Context(context, owners(i))
        materialized = i + 1
      }
      context
    }
  }

  /** A typing transformer entering owners by `atOwner`, keeping `contexts`. */
  final class ClosureTransformer(contexts: Contexts, viaFinally: Boolean) extends Transformer {
    override def atOwner[A](owner: Sym)(trans: => A): A = {
      val i = contexts.enter(owner)
      if (viaFinally) {
        try super.atOwner(owner)(trans) finally contexts.exit(i)
      } else {
        val result = super.atOwner(owner)(trans)
        contexts.exit(i)
        result
      }
    }
    override def visitDefinition(): Unit = contexts.visitDefinition()
  }

  /** A typing transformer entering owners by `pushOwner` / `popOwner`, keeping `contexts`; its `atOwner` is the base
   *  one, by way of those. */
  class PushPopTransformer(contexts: Contexts) extends Transformer {
    private[this] var frames = new Array[Int](16)
    private[this] var n = 0
    override def pushOwner(owner: Sym): Unit = {
      if (n == frames.length) frames = java.util.Arrays.copyOf(frames, n * 2)
      frames(n) = contexts.enter(owner)
      n += 1
      super.pushOwner(owner)
    }
    override def popOwner(): Unit = {
      super.popOwner()
      n -= 1
      contexts.exit(frames(n))
    }
    override def visitDefinition(): Unit = contexts.visitDefinition()
  }

  /** As `SuperAccessors`: a typing transformer that overrides `atOwner` to keep state of its own per owner. */
  final class LegacyOverride(contexts: Contexts) extends PushPopTransformer(contexts) {
    var validCurrentOwner = true
    var classes = 0
    override def atOwner[A](owner: Sym)(trans: => A): A = {
      val savedValid = validCurrentOwner
      if (owner.id % 7 == 0) { validCurrentOwner = true; classes += 1 }
      val result = super.atOwner(owner)(trans)
      validCurrentOwner = savedValid
      result
    }
  }

  /** `defs` definitions nested up to `depth` deep, each with a few expressions of a few leaves. */
  def tree(defs: Int, depth: Int, defn: (Sym, Array[Tree]) => Tree, rnd: scala.util.Random): Array[Tree] = {
    var remaining = defs
    var ids = 0
    def expr(): Tree = new Apply(Array.fill(1 + rnd.nextInt(3))(if (rnd.nextInt(4) == 0) new Apply(Array(new Leaf(0))) else new Leaf(1)))
    def definition(d: Int): Tree = {
      remaining -= 1
      ids += 1
      val nested = if (d < depth) Array.fill(math.min(remaining, rnd.nextInt(4)))(definition(d + 1)) else Array.empty[Tree]
      defn(new Sym(ids), nested ++ Array.fill(1 + rnd.nextInt(3))(expr()))
    }
    val top = scala.collection.mutable.ArrayBuffer.empty[Tree]
    while (remaining > 0) top += definition(0)
    top.toArray
  }

  def main(args: Array[String]): Unit = {
    val variant = args.headOption.getOrElse("lazy-closure")
    val readEvery = args.lift(1).fold(0)(_.toInt)
    val seconds = args.lift(2).fold(5)(_.toInt)
    val (contextsName, protocol) = variant.span(_ != '-') match { case (c, p) => (c, p.drop(1)) }
    val contexts = contextsName match {
      case "plain" => new NoContexts
      case "eager" => new EagerContexts(readEvery)
      case "lazy" => new LazyContexts(readEvery)
      case other => sys.error(s"contexts $other: expected plain, eager or lazy")
    }
    val (t, defn) = protocol match {
      case "closure" => (new ClosureTransformer(contexts, viaFinally = false), new DefnClosure(_, _))
      case "closure-finally" => (new ClosureTransformer(contexts, viaFinally = true), new DefnClosure(_, _))
      case "push-pop" => (new PushPopTransformer(contexts), new DefnPushPop(_, _))
      case "push-pop-finally" => (new PushPopTransformer(contexts), new DefnPushPopFinally(_, _))
      case "legacy" => (new LegacyOverride(contexts), new DefnPushPop(_, _))
      case other => sys.error(s"protocol $other: expected closure, closure-finally, push-pop, push-pop-finally or legacy")
    }
    println(s"$variant: ${t.getClass.getSimpleName}, pushPopOwners = ${t.pushPopOwners}")
    val trees = tree(defs = 20000, depth = 6, defn, new scala.util.Random(42))
    val threads = java.lang.management.ManagementFactory.getThreadMXBean.asInstanceOf[com.sun.management.ThreadMXBean]
    val me = Thread.currentThread.threadId
    def op(): Unit = if (t.transformTrees(trees) ne trees) sys.error("changed")
    val warmEnd = System.nanoTime + 3_000_000_000L
    while (System.nanoTime < warmEnd) op()
    for (_ <- 1 to seconds) {
      val a0 = threads.getThreadAllocatedBytes(me)
      val t0 = System.nanoTime
      var ops = 0
      while (System.nanoTime - t0 < 1_000_000_000L) { op(); ops += 1 }
      val ns = System.nanoTime - t0
      val bytes = threads.getThreadAllocatedBytes(me) - a0
      println(f"$variant%-24s readEvery=$readEvery%-3d ${ns / 1e3 / ops}%8.1f us/op ${bytes.toDouble / ops}%10.0f B/op")
    }
  }
}
