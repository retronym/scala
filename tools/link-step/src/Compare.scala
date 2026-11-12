package linkstep

import scala.jdk.CollectionConverters._
import scala.tools.asm.{ClassReader, Opcodes}
import scala.tools.asm.tree._

/** One member of a class file, keyed by name and descriptor. */
final case class Member(isField: Boolean, name: String, desc: String, access: Int, body: Option[List[String]]) {
  def key = (isField, name, desc)
  def show = s"${if (isField) "field" else "method"} $name$desc ${Compare.flags(access)}"
  /** The member with fresh-name counters (`O$lzycompute$3`) replaced by `$N`. */
  def canonical = Member(isField, Compare.canon(name), desc, access, body.map(_.map(Compare.canon)))
}

/** A difference between a regenerated class and the class the compiler emitted. `freshName` marks a difference that
 *  goes away when fresh-name counters are ignored. */
final case class Diff(cls: String, kind: String, level: Int, what: String, freshName: Boolean = false)

/** Per-class outcome. `derivedMembers` are the ancestor-derived members found in the regenerated class, with their
 *  kind; `diffs` are the mismatches at level 1 (member set, flags, `$init$` calls) and level 2 (bodies). */
final case class ClassResult(cls: String, derivedMembers: List[(String, Member)], initCalls: List[String], diffs: List[Diff]) {
  def derived: Map[String, Int] = derivedMembers.groupBy(_._1).map { case (k, v) => k -> v.size }
  def hasDerived = derivedMembers.nonEmpty || initCalls.nonEmpty
  def level1Ok = !diffs.exists(_.level == 1)
  def level2Ok = diffs.isEmpty
  def level1OkModFresh = !diffs.exists(d => d.level == 1 && !d.freshName)
  def level2OkModFresh = diffs.forall(_.freshName)
}

/** Bytecode names of a class's own pickled members: methods (with `$lzycompute` for lazy vals) and fields. A name
 *  expanded by the class's own trees (`p$C$$x`) still counts as own. */
final case class Own(names: Set[String], fieldNames: Set[String]) {
  def isOwn(name: String, field: Boolean): Boolean = {
    val set = if (field) fieldNames else names
    set(name) || { val i = name.lastIndexOf("$$"); i > 0 && set(name.substring(i + 2)) }
  }
}

object Compare {
  def read(bytes: Array[Byte]): ClassNode = {
    val cn = new ClassNode()
    new ClassReader(bytes).accept(cn, ClassReader.SKIP_FRAMES)
    cn
  }

  private val FreshLzy = """\$lzycompute\$\d+""".r
  def canon(s: String): String = FreshLzy.replaceAllIn(s, "\\$lzycompute\\$N")

  def flags(access: Int): String = {
    val names = List(Opcodes.ACC_PUBLIC -> "public", Opcodes.ACC_PRIVATE -> "private", Opcodes.ACC_PROTECTED -> "protected",
      Opcodes.ACC_STATIC -> "static", Opcodes.ACC_FINAL -> "final", Opcodes.ACC_SYNCHRONIZED -> "synchronized",
      Opcodes.ACC_VOLATILE -> "volatile/bridge", Opcodes.ACC_TRANSIENT -> "transient/varargs", Opcodes.ACC_ABSTRACT -> "abstract",
      Opcodes.ACC_SYNTHETIC -> "synthetic")
    names.collect { case (f, n) if (access & f) != 0 => n }.mkString("[", " ", "]")
  }

  def members(cn: ClassNode): List[Member] =
    cn.fields.asScala.toList.map(f => Member(isField = true, f.name, f.desc, f.access, None)) :::
      cn.methods.asScala.toList.map(m => Member(isField = false, m.name, m.desc, m.access, Some(body(m))))

  /** Instructions with labels, line numbers and frames dropped and jump targets numbered. */
  def body(m: MethodNode): List[String] = {
    val insns = m.instructions.toArray.toList
    val labels = insns.collect { case l: LabelNode => l }.zipWithIndex.toMap
    insns.flatMap {
      case _: LabelNode | _: LineNumberNode | _: FrameNode => Nil
      case i: MethodInsnNode => List(s"${op(i)} ${i.owner}.${i.name}${i.desc}${if (i.itf) " itf" else ""}")
      case i: FieldInsnNode  => List(s"${op(i)} ${i.owner}.${i.name}:${i.desc}")
      case i: VarInsnNode    => List(s"${op(i)} ${i.`var`}")
      case i: TypeInsnNode   => List(s"${op(i)} ${i.desc}")
      case i: LdcInsnNode    => List(s"${op(i)} ${i.cst}")
      case i: IntInsnNode    => List(s"${op(i)} ${i.operand}")
      case i: IincInsnNode   => List(s"${op(i)} ${i.`var`} ${i.incr}")
      case i: JumpInsnNode   => List(s"${op(i)} L${labels(i.label)}")
      case i: InvokeDynamicInsnNode => List(s"${op(i)} ${i.name}${i.desc}")
      case i: TableSwitchInsnNode   => List(s"${op(i)} ${i.min}-${i.max}")
      case i: LookupSwitchInsnNode  => List(s"${op(i)} ${i.keys}")
      case i => List(op(i))
    }
  }

  private def op(i: AbstractInsnNode): String =
    if (i.getOpcode < 0) "?" else scala.tools.asm.util.Printer.OPCODES(i.getOpcode)

  /** Members of the regenerated class that come from the stub itself, not from ancestors. */
  private def isStructural(m: Member, isTrait: Boolean, own: Own): Boolean = {
    val ownNames = own.names
    m.name == "<init>" || m.name == "<clinit>" || (isTrait && m.name == "$init$") ||
      (m.isField && m.name == "MODULE$") || m.name == "$outer" ||
      // the backend emits the class's own fields from its decls, whether or not there is a tree for them
      (m.isField && own.isOwn(m.name, field = true)) ||
      // a trait's own outer accessor
      (isTrait && m.name.endsWith("$$$outer") && (m.access & Opcodes.ACC_ABSTRACT) != 0) ||
      // accessor and field of the class's own nested objects
      (!m.isField && ownNames(m.name) && m.desc.startsWith("()") && (m.access & (Opcodes.ACC_BRIDGE | Opcodes.ACC_STATIC)) == 0) ||
      (m.isField && m.name.endsWith("$module") && ownNames(m.name.stripSuffix("$module")))
  }

  private val Specialized = """.*\$mc[A-Z]+\$sp""".r

  /** What kind of ancestor-derived member this is, from its name, flags and body. */
  def kind(m: Member): String = {
    val b = m.body.getOrElse(Nil)
    def invokes(p: String => Boolean) = b.exists(i => i.startsWith("INVOKE") && p(i))
    def reads  = b.exists(i => i.startsWith("GETFIELD") || i.startsWith("GETSTATIC"))
    def writes = b.exists(i => i.startsWith("PUTFIELD") || i.startsWith("PUTSTATIC"))
    if (m.isField) {
      if (m.name.startsWith("bitmap$")) "lazy val bitmap"
      else if (m.name.endsWith("$module")) "inherited object field"
      else "trait field"
    }
    else if ((m.access & Opcodes.ACC_BRIDGE) != 0) "bridge"
    else if ((m.access & Opcodes.ACC_STATIC) != 0) "static forwarder"
    else if (m.name.contains("$_setter_$")) "trait setter"
    else if (m.name.contains("$$super$")) "trait super accessor"
    else if (m.name.contains("$lzycompute")) {
      if (b.exists(_.contains("$module:"))) "inherited object accessor" else "trait lazy val"
    }
    else if (Specialized.matches(m.name)) "specialized forwarder"
    else if (m.name.endsWith("$$$outer")) "trait outer accessor"
    else if (b.exists(_.contains("$module:"))) "inherited object accessor"
    else if (b.exists(_.contains("bitmap$"))) "trait lazy val"
    else if (reads && b.size <= 4 && !writes) "trait field accessor"
    else if (writes && b.size <= 5) "trait field setter"
    else if (invokes(i => i.startsWith("INVOKESTATIC") && i.endsWith("itf")) ||
             invokes(i => i.startsWith("INVOKESPECIAL") && i.endsWith("itf"))) "mixin forwarder"
    else "other"
  }

  def initCalls(cn: ClassNode): List[String] =
    cn.methods.asScala.filter(_.name == "<init>").toList.flatMap(_.instructions.toArray.toList).collect {
      case i: MethodInsnNode if i.name == "$init$" => i.owner
    }.distinct

  /** Looks at a real member the regeneration did not produce: is it one we should have produced? */
  def looksDerived(m: Member, isTrait: Boolean): Option[String] = {
    if (!m.isField && (m.access & Opcodes.ACC_BRIDGE) != 0) Some("bridge")
    else if (m.name.contains("$_setter_$")) Some("trait setter")
    else if (m.name.contains("$$super$")) Some("trait super accessor")
    else if (!m.isField && Specialized.matches(m.name)) Some("specialized variant of own method")
    else if (!m.isField && !isTrait && kind(m) == "mixin forwarder") Some("mixin forwarder")
    else None
  }

  def compare(cls: String, regenerated: Array[Byte], real: Array[Byte], own: Own): ClassResult = {
    val gen = read(regenerated)
    val ref = read(real)
    val isTrait = (ref.access & Opcodes.ACC_INTERFACE) != 0
    val genMembers = members(gen)
    val refMembers = members(ref)
    val refByKey = refMembers.map(m => m.key -> m).toMap
    val genByKey = genMembers.map(m => m.key -> m).toMap
    val refByCanon = refMembers.map(m => m.canonical.key -> m.canonical).toMap
    val genByCanon = genMembers.map(m => m.canonical.key -> m.canonical).toMap
    val derived = genMembers.filterNot(isStructural(_, isTrait, own))
    val diffs = List.newBuilder[Diff]
    for (g <- derived) {
      val k = kind(g)
      val gc = g.canonical
      // compare modulo fresh-name counters; then say whether the raw names or bodies differed too
      val fresh = !refByKey.get(g.key).exists(r => r.body == g.body)
      refByCanon.get(gc.key) match {
        case None => diffs += Diff(cls, k, 1, s"regenerated only: ${g.show}")
        case Some(r) =>
          if (r.access != gc.access) diffs += Diff(cls, k, 1, s"flags: ${g.show} vs real ${flags(r.access)}")
          else if (r.body != gc.body) diffs += Diff(cls, k, 2, s"body: ${g.name}${g.desc}\n      regenerated: ${g.body.getOrElse(Nil).mkString("; ")}\n      real:        ${refMembers.find(_.canonical.key == gc.key).flatMap(_.body).getOrElse(Nil).mkString("; ")}")
          else if (fresh) diffs += Diff(cls, k, if (refByKey.contains(g.key)) 2 else 1, s"fresh name: ${g.name}${g.desc} is ${refMembers.find(_.canonical.key == gc.key).map(_.name).getOrElse("?")} in the real class", freshName = true)
      }
    }
    // real members that look ancestor-derived but were not regenerated
    for (r <- refMembers if !genByCanon.contains(r.canonical.key)  && !own.isOwn(r.name, r.isField)) {
      looksDerived(r, isTrait).foreach(k => diffs += Diff(cls, k, 1, s"real only: ${r.show}"))
    }
    val genInit = initCalls(gen)
    val refInit = initCalls(ref)
    if (genInit != refInit) diffs += Diff(cls, "$init$ calls", 1, s"regenerated ${genInit.mkString(",")} vs real ${refInit.mkString(",")}")
    ClassResult(cls, derived.map(m => kind(m) -> m), genInit, diffs.result())
  }
}
