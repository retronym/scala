package w4p

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

sealed trait Ty
case object TInt extends Ty
case object TLong extends Ty
case object TDouble extends Ty
case object TStr extends Ty
case object TBool extends Ty
case class TVC(name: String, targs: List[Ty]) extends Ty
case class TP(name: String) extends Ty
case class TOpt(t: Ty) extends Ty
case class TTup(a: Ty, b: Ty) extends Ty
case class TArr(t: Ty) extends Ty
case class TFun(a: Ty, b: Ty) extends Ty
case class TSam(a: Ty, b: Ty) extends Ty
case class TSeq(t: Ty) extends Ty
case class TList(t: Ty) extends Ty
/** The sealed hierarchy Sh of the prelude. */
case object TAdt extends Ty

case class Param(name: String, ty: Ty, byName: Boolean = false, vararg: Boolean = false, default: Option[String] = None)
case class TPDef(name: String, spec: String, ctag: Boolean)
case class Meth(idx: Int, name: String, kind: String, tps: List[TPDef], params: List[Param], ret: Ty,
                render: Option[List[String] => String] = None, isAbstract: Boolean = false)
case class Var(name: String, ty: Ty, mutable: Boolean = false)
case class Env(vars: List[Var], meths: List[Meth], ctags: Set[String], tps: Set[String], inVC: Boolean = false)
case class VCDef(name: String, tparam: Option[String], under: Ty)
class GenFail(msg: String) extends RuntimeException(msg)

/** Options restrict the grammar (used for the Scala 3 comparison and to switch features off). */
case class GenOpts(scala3: Boolean = false)

class Gen(seed: Long, opts: GenOpts = GenOpts()) {
  val r = new Random(seed)
  val vcs = ArrayBuffer[VCDef]()
  var counter = 0
  def fresh(p: String): String = { counter += 1; p + counter }
  def pick[A](xs: collection.Seq[A]): A = xs(r.nextInt(xs.length))
  def chance(p: Double): Boolean = r.nextDouble() < p

  def show(t: Ty): String = t match {
    case TInt => "Int"; case TLong => "Long"; case TDouble => "Double"; case TStr => "String"; case TBool => "Boolean"; case TAdt => "Sh"
    case TVC(n, Nil) => n
    case TVC(n, ts) => n + ts.map(show).mkString("[", ", ", "]")
    case TP(n) => n
    case TOpt(a) => s"Option[${show(a)}]"
    case TTup(a, b) => s"(${show(a)}, ${show(b)})"
    case TArr(a) => s"Array[${show(a)}]"
    case TFun(a, b) => s"Function1[${show(a)}, ${show(b)}]"
    case TSam(a, b) => s"Sam0[${show(a)}, ${show(b)}]"
    case TSeq(a) => s"Seq[${show(a)}]"
    case TList(a) => s"List[${show(a)}]"
  }
  def subst(t: Ty, m: Map[String, Ty]): Ty = t match {
    case TP(n) => m.getOrElse(n, t)
    case TVC(n, ts) => TVC(n, ts.map(subst(_, m)))
    case TOpt(a) => TOpt(subst(a, m)); case TTup(a, b) => TTup(subst(a, m), subst(b, m))
    case TArr(a) => TArr(subst(a, m)); case TFun(a, b) => TFun(subst(a, m), subst(b, m))
    case TSam(a, b) => TSam(subst(a, m), subst(b, m)); case TSeq(a) => TSeq(subst(a, m)); case TList(a) => TList(subst(a, m))
    case _ => t
  }
  def substM(m: Meth, s: Map[String, Ty]): Meth =
    m.copy(params = m.params.map(p => p.copy(ty = subst(p.ty, s))), ret = subst(m.ret, s))
  def tpsOf(t: Ty): Set[String] = t match {
    case TP(n) => Set(n)
    case TVC(_, ts) => ts.flatMap(tpsOf).toSet
    case TOpt(a) => tpsOf(a); case TArr(a) => tpsOf(a); case TSeq(a) => tpsOf(a); case TList(a) => tpsOf(a)
    case TTup(a, b) => tpsOf(a) ++ tpsOf(b); case TFun(a, b) => tpsOf(a) ++ tpsOf(b); case TSam(a, b) => tpsOf(a) ++ tpsOf(b)
    case _ => Set()
  }
  def under(t: TVC): Ty = { val d = vcs.find(_.name == t.name).get; d.tparam.fold(d.under)(p => subst(d.under, Map(p -> t.targs.head))) }
  /** One-way matching of a pattern mentioning the names in `free` against a type. */
  def matchTy(pat: Ty, t: Ty, free: Set[String], acc: Map[String, Ty]): Option[Map[String, Ty]] = (pat, t) match {
    case (TP(n), _) if free(n) => acc.get(n) match { case Some(x) => if (x == t) Some(acc) else None; case None => Some(acc + (n -> t)) }
    case (TOpt(a), TOpt(b)) => matchTy(a, b, free, acc)
    case (TArr(a), TArr(b)) => matchTy(a, b, free, acc)
    case (TList(a), TList(b)) => matchTy(a, b, free, acc)
    case (TTup(a, b), TTup(c, d)) => matchTy(a, c, free, acc).flatMap(matchTy(b, d, free, _))
    case _ => if (pat == t) Some(acc) else None
  }
  def erasureKey(t: Ty): String = t match {
    case TInt => "I"; case TLong => "J"; case TDouble => "D"; case TStr => "S"; case TBool => "Z"
    case v: TVC => val d = vcs.find(_.name == v.name).get; if (d.tparam.nonEmpty) "O" else erasureKey(d.under)
    case TP(_) => "O"
    case other => other.getClass.getSimpleName
  }
  def arrayable(t: Ty, env: Env): Boolean = t match { case TArr(_) => false; case _ => tpsOf(t).isEmpty || (t match { case TP(n) => env.ctags(n); case _ => false }) }
  def showable(t: Ty): Boolean = t match {
    case TFun(_, _) | TSam(_, _) | TArr(_) | TSeq(_) => false
    case TOpt(a) => showable(a); case TList(a) => showable(a); case TTup(a, b) => showable(a) && showable(b)
    case _ => true
  }
  def checkable(t: Ty): Boolean = t match { case TInt | TLong | TDouble | TStr => true; case TVC(_, Nil) => true; case _ => false }

  def baseTy(): Ty = {
    val prim = List(TInt, TInt, TLong, TDouble, TStr, TStr, TAdt)
    if (vcs.nonEmpty && chance(0.2)) { val d = pick(vcs); TVC(d.name, d.tparam.toList.map(_ => pick(prim))) } else pick(prim)
  }
  def genTy(env: Env, d: Int, needShow: Boolean): Ty = {
    val tps = env.tps.toList.sorted
    def leaf(): Ty = if (tps.nonEmpty && chance(0.45)) TP(pick(tps)) else baseTy()
    if (d <= 0 || chance(0.5)) leaf()
    else r.nextInt(if (needShow) 4 else 7) match {
      case 0 => TOpt(genTy(env, d - 1, needShow))
      case 1 => TTup(genTy(env, d - 1, needShow), genTy(env, d - 1, needShow))
      case 2 => TList(genTy(env, d - 1, needShow))
      case 3 => leaf()
      case 4 => val e = leaf(); if (arrayable(e, env)) TArr(e) else e
      case 5 => TFun(genTy(env, d - 1, true), genTy(env, d - 1, true))
      case 6 => TSam(genTy(env, d - 1, true), genTy(env, d - 1, true))
    }
  }
  /** Return types: printable, or an array of something printable. */
  def retTy(env: Env): Ty = { val t = genTy(env, 2, needShow = true); if (chance(0.12) && arrayable(t, env)) TArr(t) else t }

  def lit(t: Ty): String = t match {
    case TInt => r.nextInt(9).toString
    case TLong => r.nextInt(9).toString + "L"
    case TDouble => pick(List("0.5", "1.0", "2.5", "3.0"))
    case TStr => "\"" + pick(List("a", "bc", "", "xyz")) + "\""
    case TBool => pick(List("true", "false"))
    case TAdt => "(K2: Sh)"
    case _ => sys.error("lit")
  }

  def call(m: Meth, s: Map[String, Ty], env: Env, d: Int, prefix: String): String = {
    val ps0 = m.params.map(p => p.copy(ty = subst(p.ty, s)))
    val ps = if (ps0.nonEmpty && ps0.last.default.nonEmpty && chance(0.5)) ps0.init else ps0
    val args = ps.flatMap { p =>
      if (p.vararg) List.fill(r.nextInt(3))(gen(p.ty, env, d - 1)) else List(gen(p.ty, env, d - 1))
    }
    m.render match {
      case Some(f) => f(args)
      case None =>
        val targs = if (m.tps.isEmpty) "" else m.tps.map(tp => show(s(tp.name))).mkString("[", ", ", "]")
        val named = args.length == ps.length && ps.forall(!_.vararg) && ps.length > 1 && chance(0.25)
        val as0 = if (named) r.shuffle(ps.map(_.name).zip(args).map { case (n, a) => s"$n = $a" }) else args
        val as = if (m.kind == "def" && (m.params.nonEmpty || m.name.startsWith("m"))) as0.mkString("(", ", ", ")") else ""
        prefix + m.name + targs + as
    }
  }
  /** Instantiations of the method's own type parameters under which it returns `t`. */
  def instFor(m: Meth, t: Ty, env: Env): Option[Map[String, Ty]] = {
    val free = m.tps.map(_.name).toSet
    matchTy(m.ret, t, free, Map()).flatMap { s0 =>
      val s = m.tps.foldLeft(s0)((acc, tp) => if (acc.contains(tp.name)) acc else acc + (tp.name -> baseTy()))
      if (m.tps.forall(tp => !tp.ctag || arrayable(s(tp.name), env))) Some(s) else None
    }
  }

  def gen(t: Ty, env: Env, d: Int): String = {
    val leaves = ArrayBuffer[() => String]()
    val nodes = ArrayBuffer[() => String]()
    def g(t2: Ty): String = gen(t2, env, d - 1)
    def other(): Ty = genTy(env.copy(tps = Set()), 0, needShow = true)
    env.vars.foreach { v =>
      if (v.ty == t) { leaves += (() => v.name); leaves += (() => v.name) }
      else v.ty match {
        case TOpt(`t`) => nodes += (() => s"${v.name}.getOrElse(${g(t)})")
        case TSeq(`t`) => nodes += (() => s"${v.name}.headOption.getOrElse(${g(t)})")
        case TList(`t`) => nodes += (() => s"${v.name}.headOption.getOrElse(${g(t)})")
        case TArr(`t`) => nodes += (() => s"${v.name}.apply(0)")
        case TTup(`t`, _) => nodes += (() => s"${v.name}._1")
        case TTup(_, `t`) => nodes += (() => s"${v.name}._2")
        case TFun(a, `t`) => nodes += (() => s"${v.name}.apply(${g(a)})")
        case TSam(a, `t`) => nodes += (() => s"${v.name}.ap(${g(a)})")
        case vc: TVC if under(vc) == t => nodes += (() => s"${v.name}.u")
        case TSeq(_) if t == TInt => nodes += (() => s"${v.name}.length")
        case TSeq(e) if t == TList(e) => nodes += (() => s"${v.name}.toList")
        case _ =>
      }
    }
    env.meths.foreach { m =>
      instFor(m, t, env).foreach { s =>
        if (m.params.isEmpty && m.tps.isEmpty && m.render.isEmpty) leaves += (() => call(m, s, env, d, ""))
        else { nodes += (() => call(m, s, env, d, "")); nodes += (() => call(m, s, env, d, "")) }
      }
    }
    t match {
      case TInt | TLong | TDouble =>
        leaves += (() => lit(t))
        nodes += (() => s"(${g(t)} + ${g(t)})")
        nodes += (() => { val (i, acc) = (fresh("i"), fresh("acc")); s"({ var $i = 0; var $acc: ${show(t)} = ${g(t)}; while ($i < 3) { $acc = $acc + ${gen(t, env.copy(vars = Var(acc, t) :: Var(i, TInt) :: env.vars), 0)}; $i += 1 }; $acc })" })
        nodes += (() => s"List[${show(t)}](${g(t)}, ${g(t)}).max")
        if (t == TInt) nodes += (() => { val (i, p, q) = (fresh("i"), fresh("x"), fresh("x")); s"(for ($i <- 0 until 3; ($p, $q) = ($i, ${g(TInt)}) if $p != 1) yield $p + $q).sum" })
        if (t == TInt) nodes += (() => { val (a, i) = (fresh("arr"), fresh("i")); s"({ val $a = new Array[Int](4); var $i = 0; while ($i < $a.length) { $a($i) = $i * ${g(TInt)}; $i += 1 }; $a(2) })" })
        if (t == TInt) nodes += (() => s"${g(TInt)}.dbl")
        if (t == TInt) nodes += (() => s"Imp.withImp(${g(TInt)})")
        if (t == TInt) nodes += (() => s"${g(TStr)}.length")
        if (t == TLong) nodes += (() => s"${g(TInt)}.toLong")
        if (t == TDouble) nodes += (() => s"${g(TInt)}.toDouble")
      case TStr =>
        leaves += (() => lit(t))
        nodes += (() => { val o = genTy(env, 1, needShow = true); s"""("" + ${g(o)})""" })
        nodes += (() => s"(${g(TStr)} + ${g(TStr)})")
        nodes += (() => { val o = genTy(env, 1, needShow = true); val (a, b) = (fresh("x"), fresh("x")); "({ val " + a + ": " + show(o) + " = " + g(o) + "; val " + b + ": Int = " + g(TInt) + "; s\"<$" + a + "|${" + b + " + 1}>\" })" })
      case TBool =>
        leaves += (() => lit(t))
        nodes += (() => s"(${g(TInt)} > ${g(TInt)})")
        nodes += (() => { val (e, o) = (fresh("ev"), fresh("od")); s"({ def $e(n: Int): Boolean = if (n <= 0) true else $o(n - 1); def $o(n: Int): Boolean = if (n <= 0) false else $e(n - 1); $e(${g(TInt)} + 2) })" })
        nodes += (() => { val o = genTy(env, 1, needShow = true); s"(${g(o)} == ${g(o)})" })
      case vc @ TVC(n, targs) =>
        val ta = if (targs.isEmpty) "" else targs.map(show).mkString("[", ", ", "]")
        leaves += (() => s"$n$ta(${g(under(vc))})")
      case TAdt =>
        leaves += (() => "(K2: Sh)")
        leaves += (() => s"(K0(${g(TInt)}, ${g(TStr)}): Sh)")
        nodes += (() => s"(K1(${g(TAdt)}, ${g(TLong)}): Sh)")
      case TP(_) =>
      case TOpt(a) =>
        leaves += (() => s"(None: ${show(t)})")
        leaves += (() => s"Option[${show(a)}](${g(a)})")
        nodes += (() => { val (o1, o2) = (other(), other()); val (x, y) = (fresh("x"), fresh("x")); val e2 = env.copy(vars = Var(x, o1) :: Var(y, o2) :: env.vars); s"(for ($x <- ${g(TOpt(o1))}; $y <- ${g(TOpt(o2))}) yield ${gen(a, e2, d - 1)})" })
        nodes += (() => { val x = fresh("x"); s"({ case $x if $x > 1 => ${gen(a, env.copy(vars = Var(x, TInt) :: env.vars), d - 1)} }: PartialFunction[Int, ${show(a)}]).lift(${g(TInt)})" })
        nodes += (() => { val x = fresh("x"); s"${g(t)}.map((($x: ${show(a)}) => ${gen(a, env.copy(vars = Var(x, a) :: env.vars), d - 1)}))" })
      case TTup(a, b) =>
        leaves += (() => s"(${g(a)}, ${g(b)})")
        if (a == b) nodes += (() => s"${g(a)}.pairUp")
      case TList(a) =>
        nodes += (() => { val o = other(); val x = fresh("x"); val e2 = env.copy(vars = Var(x, o) :: env.vars); s"(for ($x <- ${g(TList(o))}; if ${gen(TBool, e2, d - 1)}) yield ${gen(a, e2, d - 1)})" })
        nodes += (() => { val x = fresh("x"); s"List[Int](1, 2, 3, 4).collect { case $x if $x % 2 == 0 => ${gen(a, env.copy(vars = Var(x, TInt) :: env.vars), d - 1)} }" })
        leaves += (() => s"List[${show(a)}](${List.fill(r.nextInt(3))(g(a)).mkString(", ")})")
      case TArr(a) =>
        if (!arrayable(a, env) && leaves.isEmpty) throw new GenFail("array of " + a)
        if (arrayable(a, env)) leaves += (() => s"Array[${show(a)}](${List.fill(1 + r.nextInt(2))(g(a)).mkString(", ")})")
      case TFun(a, b) =>
        leaves += (() => { val x = fresh("x"); s"(($x: ${show(a)}) => ${gen(b, env.copy(vars = Var(x, a) :: env.vars), d - 1)})" })
      case TSam(a, b) =>
        leaves += (() => { val x = fresh("x"); s"((($x: ${show(a)}) => ${gen(b, env.copy(vars = Var(x, a) :: env.vars), d - 1)}): ${show(t)})" })
      case TSeq(_) => sys.error("seq requested")
    }
    if (d > 0) {
      nodes += (() => s"(if (${g(TBool)}) ${g(t)} else ${g(t)})")
      nodes += (() => s"${g(TOpt(t))}.getOrElse(${g(t)})")
      nodes += (() => { val o = other(); if (chance(0.5)) s"${g(TTup(t, o))}._1" else s"${g(TTup(o, t))}._2" })
      nodes += (() => { val o = other(); s"${g(TFun(o, t))}.apply(${g(o)})" })
      nodes += (() => { val o = other(); s"${g(TSam(o, t))}.ap(${g(o)})" })
      if (arrayable(t, env)) nodes += (() => s"${g(TArr(t))}.apply(0)")
      if (checkable(t)) nodes += (() => { val o = other(); val x = fresh("x"); s"((${g(o)}: Any) match { case $x: ${show(t)} => $x; case _ => ${g(t)} })" })
      nodes += (() => block(t, env, d)); nodes += (() => block(t, env, d))
      def w(vs: Var*): Env = env.copy(vars = vs.toList ::: env.vars)
      nodes += (() => s"Tr.t(${fresh("")}, ${g(t)})"); nodes += (() => s"Tr.t(${fresh("")}, ${g(t)})")
      nodes += (() => { val (a, b, c, n, m) = (fresh("x"), fresh("x"), fresh("x"), fresh("x"), fresh("x"))
        s"(${g(TAdt)} match { case K0($a, $b) if $a > 2 => ${gen(t, w(Var(a, TInt), Var(b, TStr)), d - 1)}; case K1(K0($c, _), $n) => ${gen(t, w(Var(c, TInt), Var(n, TLong)), d - 1)}; case K1(_, $m) => ${gen(t, w(Var(m, TLong)), d - 1)}; case K2 | K0(_, _) => ${g(t)} })" })
      nodes += (() => { val (a, b, c) = (fresh("x"), fresh("x"), fresh("x"))
        s"""((${g(TInt)}, ${g(TStr)}) match { case (0, $a) => ${gen(t, w(Var(a, TStr)), d - 1)}; case ($b, "a" | "bc") => ${gen(t, w(Var(b, TInt)), d - 1)}; case ($b, $c) if $b > $c.length => ${gen(t, w(Var(b, TInt), Var(c, TStr)), d - 1)}; case _ => ${g(t)} })""" })
      nodes += (() => { val o = other(); val x = fresh("x")
        s"(${g(TOpt(o))} match { case Some($x) if ${gen(TBool, w(Var(x, o)), d - 1)} => ${gen(t, w(Var(x, o)), d - 1)}; case Some(_) => ${g(t)}; case None => ${g(t)} })" })
      nodes += (() => { val (h, n) = (fresh("x"), fresh("x"))
        s"(${g(TInt)} match { case Ev(Ev($h)) => ${gen(t, w(Var(h, TInt)), d - 1)}; case Ev($h) => ${gen(t, w(Var(h, TInt)), d - 1)}; case $n => ${gen(t, w(Var(n, TInt)), d - 1)} })" })
      nodes += (() => s"""(try { if (${g(TBool)}) throw new IllegalStateException("x"); ${g(t)} } catch { case _: IllegalStateException => ${g(t)} } finally { Tr.t(${fresh("")}, 0) })""")
      nodes += (() => { val f = fresh("loc"); s"({ def $f(): ${show(t)} = { try { if (${g(TBool)}) return ${g(t)}; ${g(t)} } finally { Tr.t(${fresh("")}, 0) } }; $f() })" })
      nodes += (() => { val (f, i, acc) = (fresh("loop"), fresh("i"), fresh("acc"))
        s"({ @scala.annotation.tailrec def $f($i: Int, $acc: ${show(t)}): ${show(t)} = if ($i <= 0) $acc else $f($i - 1, ${gen(t, w(Var(acc, t), Var(i, TInt)), d - 1)}); $f(2, ${g(t)}) })" })
      nodes += (() => { val x = fresh("x"); s"({ case $x if $x > 1 => ${gen(t, w(Var(x, TInt)), d - 1)} }: PartialFunction[Int, ${show(t)}]).applyOrElse(${g(TInt)}, (($x: Int) => ${g(t)}))" })
      env.vars.filter(_.mutable).foreach(v => nodes += (() => s"({ ${v.name} = ${g(v.ty)}; ${g(t)} })"))
    }
    if (leaves.isEmpty && nodes.isEmpty) throw new GenFail("no way to make " + show(t))
    if (leaves.isEmpty) { if (d < -6) throw new GenFail("depth " + show(t)); pick(nodes)() }
    else if (d <= 0 || nodes.isEmpty || chance(0.25)) pick(leaves)() else pick(nodes)()
  }

  def block(t: Ty, env0: Env, d: Int): String = {
    var env = env0
    val sb = new StringBuilder("({ ")
    for (_ <- 0 until 1 + r.nextInt(2)) {
      r.nextInt(if (env.inVC) 5 else 6) match {
        case 0 => val x = fresh("x"); val ty = genTy(env, 1, false); sb ++= s"val $x: ${show(ty)} = ${gen(ty, env, d - 1)}; "; env = env.copy(vars = Var(x, ty) :: env.vars)
        case 1 => val x = fresh("x"); val ty = genTy(env, 1, false); sb ++= s"lazy val $x: ${show(ty)} = ${gen(ty, env, d - 1)}; "; env = env.copy(vars = Var(x, ty) :: env.vars)
        case 2 => val x = fresh("x"); val ty = genTy(env, 1, true); sb ++= s"var $x: ${show(ty)} = ${gen(ty, env, d - 1)}; "; env = env.copy(vars = Var(x, ty, mutable = true) :: env.vars)
        case 3 | 4 =>
          val f = fresh("loc"); val ret = genTy(env, 1, false)
          val p1 = Param(fresh("p"), genTy(env, 1, false), byName = chance(0.4))
          val dty = genTy(env, 0, true)
          val ps = if (chance(0.5)) List(p1, Param(fresh("p"), dty, default = Some(gen(dty, env, 0)))) else List(p1)
          val benv = env.copy(vars = ps.map(p => Var(p.name, p.ty)) ::: env.vars)
          sb ++= s"def $f(${ps.map(showParam).mkString(", ")}): ${show(ret)} = ${gen(ret, benv, d - 1)}; "
          env = env.copy(meths = Meth(0, f, "def", Nil, ps, ret, render = Some(as => s"$f(${as.mkString(", ")})")) :: env.meths)
        case 5 =>
          val c = fresh("L"); val ret = genTy(env, 1, false)
          val p = Param(fresh("p"), genTy(env, 1, false)); val q = Param(fresh("p"), genTy(env, 1, false))
          val benv = env.copy(vars = Var(p.name, p.ty) :: Var(q.name, q.ty) :: env.vars)
          sb ++= s"class $c(${showParam(p)}) { def g(${showParam(q)}): ${show(ret)} = ${gen(ret, benv, d - 1)} }; "
          env = env.copy(meths = Meth(0, c, "def", Nil, List(p, q), ret, render = Some(as => s"new $c(${as(0)}).g(${as(1)})")) :: env.meths)
      }
    }
    sb ++= gen(t, env, d - 1) ++= " })"
    sb.toString
  }
  def showParam(p: Param): String =
    p.name + ": " + (if (p.byName) "=> " else "") + show(p.ty) + (if (p.vararg) "*" else "") + p.default.fold("")(" = " + _)
  def showTps(tps: List[TPDef]): String =
    if (tps.isEmpty) "" else tps.map(tp => tp.spec + tp.name + (if (tp.ctag) ": ClassTag" else "")).mkString("[", ", ", "]")
  def specAnn(): String = if (!chance(Gen.specWeight)) "" else pick(List("@specialized(Int, Long, Double) ", "@specialized(Int, Long, Double) ", "@specialized ", "@specialized(Int) "))

  def sig(m: Meth): String = m.kind match {
    case "def" => s"def ${m.name}${showTps(m.tps)}${if (m.params.nonEmpty || m.name.startsWith("m")) m.params.map(showParam).mkString("(", ", ", ")") else ""}: ${show(m.ret)}"
    case k => s"$k ${m.name}: ${show(m.ret)}"
  }
  /** A fresh method signature. `idx` orders members: a body only calls members with a smaller index, so programs terminate. */
  def newMeth(idx: Int, env: Env, name: String = null): Meth = {
    val tps = if (chance(0.25)) List(TPDef(fresh("B"), specAnn(), chance(0.5))) else Nil
    val env2 = env.copy(tps = env.tps ++ tps.map(_.name), ctags = env.ctags ++ tps.filter(_.ctag).map(_.name))
    val n = r.nextInt(3)
    var ps = tps.map(tp => Param(fresh("p"), TP(tp.name))) ::: List.fill(n)(Param(fresh("p"), genTy(env2, 2, false)))
    ps = ps.map(p => if (chance(0.15)) p.copy(byName = true) else p)
    if (ps.nonEmpty && chance(0.2)) { val l = ps.last; if (!l.byName && tpsOf(l.ty).isEmpty && showable(l.ty)) ps = ps.init :+ l.copy(default = Some(gen(l.ty, Env(Nil, Nil, Set(), Set()), 1))) }
    else if (chance(0.2)) ps = ps :+ Param(fresh("p"), genTy(env2, 1, true), vararg = true)
    Meth(idx, if (name == null) fresh("m") else name, "def", tps, ps, retTy(env2))
  }
  def bodyEnv(m: Meth, env: Env): Env = env.copy(
    vars = m.params.map(p => Var(p.name, if (p.vararg) TSeq(p.ty) else p.ty)) ::: env.vars,
    meths = env.meths.filter(_.idx < m.idx), tps = env.tps ++ m.tps.map(_.name), ctags = env.ctags ++ m.tps.filter(_.ctag).map(_.name))

  // ---- templates ----
  class Tpl(val kind: String, val name: String, val tps: List[TPDef], val parent: Option[(Tpl, List[Ty])]) {
    val lines = ArrayBuffer[String]()
    var meths: List[Meth] = Nil // all members, in this template's own type terms
    def isLeaf = kind == "class"
  }
  var nextIdx = 0
  def idx(): Int = { nextIdx += 1; nextIdx }

  def genTpl(kind: String, name: String, parent: Option[Tpl]): Tpl = {
    val ntp = if (kind == "class") r.nextInt(2) else 1 + r.nextInt(2)
    val names = List("A", "B").map(_ + name)
    var tps = List.tabulate(ntp)(i => TPDef(names(i), specAnn(), kind != "trait" && chance(0.5)))
    val pargs = parent.map { p =>
      p.tps.map { ptp =>
        val own = tps.filter(tp => !ptp.ctag || tp.ctag)
        if (own.nonEmpty && chance(0.6)) TP(pick(own).name) else if (chance(0.7)) pick(List(TInt, TLong, TDouble)) else baseTy()
      }
    }
    val t = new Tpl(kind, name, tps, parent.map(p => (p, pargs.get)))
    val inh: List[Meth] = parent.toList.flatMap(p => p.meths.map(substM(_, p.tps.map(_.name).zip(pargs.get).toMap)))
    val zs = tps.map(tp => Meth(-1, "z" + tp.name, "def", Nil, Nil, TP(tp.name), isAbstract = !t.isLeaf))
    var all = inh ::: zs
    if (!t.isLeaf) zs.foreach(z => t.lines += s"  def ${z.name}: ${show(z.ret)}")
    val cenv = Env(Nil, Nil, tps.filter(_.ctag).map(_.name).toSet, tps.map(_.name).toSet)
    def env = cenv.copy(meths = all)
    // inherited members: implement or override
    inh.foreach { m =>
      if (m.idx == -1) { if (t.isLeaf) all = all.map(x => if (x eq m) m.copy(isAbstract = false) else x) }
      else if (m.kind == "def" && ((m.isAbstract && (t.isLeaf || chance(0.4))) || (!m.isAbstract && chance(0.3)))) {
        val be = bodyEnv(m, env)
        val body =
          if (!m.isAbstract && chance(0.6) && (m.tps.isEmpty || Gen.superPoly)) {
            val x = fresh("x")
            val sup = s"super.${m.name}${if (m.tps.isEmpty) "" else m.tps.map(_.name).mkString("[", ", ", "]")}(${m.params.map(p => if (p.vararg) p.name + ": _*" else p.name).mkString(", ")})"
            if (m.ret.isInstanceOf[TArr]) sup else s"{ val $x: ${show(m.ret)} = $sup; ${gen(m.ret, be.copy(vars = Var(x, m.ret) :: be.vars), 2)} }"
          } else gen(m.ret, be, 3)
        // default arguments are inherited, not restated
        val m2 = m.copy(params = m.params.map(_.copy(default = None)))
        t.lines += s"  override ${sig(m2)} = $body"
        all = all.map(x => if (x eq m) m.copy(isAbstract = false) else x)
      }
    }
    val leafVars = if (t.isLeaf) all.filter(_.idx == -1).map(z => Var(z.name, z.ret)) else Nil
    var eager: List[Var] = leafVars
    for (_ <- 0 until 2 + r.nextInt(3)) {
      r.nextInt(10) match {
        case 0 | 1 => // eager val or var: initialiser sees constructor parameters and earlier eager members of this template only
          val k = if (chance(0.5)) "val" else "var"
          val e = Env(eager, Nil, if (t.isLeaf) cenv.ctags else Set(), if (t.isLeaf) cenv.tps else Set())
          val ty = retTy(e)
          val m = Meth(idx(), fresh("v"), k, Nil, Nil, ty)
          t.lines += s"  ${sig(m)} = ${gen(ty, e, 2)}"
          all = all :+ m; eager = Var(m.name, ty) :: eager
        case 2 =>
          val ty = retTy(env); val m = Meth(idx(), fresh("lz"), "lazy val", Nil, Nil, ty)
          t.lines += s"  ${sig(m)} = ${gen(ty, env, 3)}"
          all = all :+ m
        case 4 if kind != "class" || tps.isEmpty || true => // nested object, initialised on first use
          val ty = retTy(env); val o = fresh("O"); val m = Meth(idx(), fresh("m"), "def", Nil, Nil, ty)
          t.lines += s"  object $o { val a: ${show(ty)} = Tr.t(${fresh("")}, ${gen(ty, env.copy(meths = env.meths.filter(_.idx < m.idx)), 2)}) }; ${sig(m)} = $o.a"
          all = all :+ m
        case 3 => // overloads, distinct after erasure
          val nm = fresh("ov"); val ret = retTy(env)
          var seen = Set[String]()
          for (_ <- 0 until 2) {
            val ps = List.fill(1 + r.nextInt(2))(Param(fresh("p"), baseTy()))
            val key = ps.map(p => erasureKey(p.ty)).mkString
            if (!seen(key)) {
              seen += key
              val m = Meth(idx(), nm, "def", Nil, ps, ret)
              t.lines += s"  ${sig(m)} = ${gen(ret, bodyEnv(m, env), 3)}"
              all = all :+ m
            }
          }
        case _ =>
          val m = newMeth(idx(), env)
          if (!t.isLeaf && chance(0.3)) { t.lines += s"  ${sig(m)}"; all = all :+ m.copy(isAbstract = true) }
          else { t.lines += s"  ${sig(m)} = ${gen(m.ret, bodyEnv(m, env), 3)}"; all = all :+ m }
      }
    }
    t.meths = all
    t
  }
  def header(t: Tpl): String = {
    val ctor = if (t.isLeaf) t.meths.filter(_.idx == -1).map(z => s"val ${z.name}: ${show(z.ret)}").mkString("(", ", ", ")") else ""
    val ext = t.parent.fold("")(p => s" extends ${p._1.name}${p._2.map(show).mkString("[", ", ", "]")}")
    s"${t.kind} ${t.name}${showTps(t.tps)}$ctor$ext {"
  }

  def genVC(i: Int): List[String] = {
    val name = "V" + i
    val generic = chance(0.3)
    val u: Ty = if (generic) TP("T" + i) else pick(List(TInt, TInt, TLong, TStr, TDouble))
    val d = VCDef(name, if (generic) Some("T" + i) else None, u)
    vcs += d
    val lines = ArrayBuffer[String]()
    val tps = d.tparam.toSet
    val withU = chance(0.5)
    lines += s"case class $name${d.tparam.fold("")("[" + _ + "]")}(u: ${show(u)}) extends AnyVal${if (withU) s" with U0[${show(u)}]" else ""} {"
    if (withU) lines += s"  def uu: ${show(u)} = u"
    var all: List[Meth] = Nil
    for (_ <- 0 until 1 + r.nextInt(3)) {
      val env = Env(List(Var("u", u)), all, Set(), tps, inVC = true)
      val m = newMeth(idx(), env)
      lines += s"  ${sig(m)} = ${gen(m.ret, bodyEnv(m, env), 3)}"
      all = all :+ m
    }
    lines += "}"
    vcMeths(name) = all
    lines.toList
  }
  val vcMeths = collection.mutable.LinkedHashMap[String, List[Meth]]()

  def exercise(label: String, recv: String, m: Meth, out: ArrayBuffer[String]): Unit = {
    val env = Env(Nil, Nil, Set(), Set())
    val s: Map[String, Ty] = m.tps.map(tp => tp.name -> (if (chance(0.6)) pick(List(TInt, TLong, TDouble)) else baseTy())).toMap
    val ret = subst(m.ret, s)
    val e = call(m, s, env, 2, recv + ".")
    val shown = ret match { case TArr(_) => "r.toList"; case _ => "r" }
    val pre = if (m.kind == "var") s"$recv.${m.name} = ${gen(ret, env, 2)}; " else ""
    out += s"""    try { ${pre}val r: ${show(ret)} = $e; sb.append("$label.${m.name}=" + $shown) } catch { case e: Throwable => sb.append("$label.${m.name}!" + e.getClass.getName) }; sb.append(" [" + Tr.take() + "]\\n")"""
  }

  /** Units of the program, in dependency order; each unit is a list of lines. */
  def program(): List[List[String]] = {
    val units = ArrayBuffer[List[String]]()
    val imp0 = "import scala.reflect.ClassTag"
    val imp = "import scala.reflect.ClassTag; import Imp._"
    units += List(imp0, "trait U0[A] extends Any { def uu: A; def uc(x: Int): (A, Int) = (uu, x) }")
    units += List(imp0, s"trait Sam0[${specAnn()}A, ${specAnn()}B] { def ap(a: A): B${if (chance(0.3)) "; def twice(a: A): (B, B) = (ap(a), ap(a))" else ""} }")
    units += List(imp0,
      "object Tr { val sb = new java.lang.StringBuilder; def t[T](k: Int, v: T): T = { sb.append(k).append(' '); v }; def take(): String = { val s = sb.toString; sb.setLength(0); s } }",
      "sealed trait Sh",
      "case class K0(a: Int, b: String) extends Sh",
      "case class K1(x: Sh, n: Long) extends Sh",
      "case object K2 extends Sh",
      "object Ev { def unapply(i: Int): Option[Int] = if (i % 2 == 0) Some(i / 2) else None }",
      "object Imp { implicit class RichI(private val self: Int) extends AnyVal { def dbl: Int = self * 2 }; implicit class Pairer[T](private val self: T) { def pairUp: (T, T) = (self, self) }; implicit val defaultK: K0 = K0(7, \"imp\"); def withImp(x: Int)(implicit k: K0): Int = x + k.a }")
    for (i <- 0 until r.nextInt(3)) units += imp :: genVC(i)
    val tpls = ArrayBuffer[Tpl]()
    val t0 = genTpl("trait", "T0", None); tpls += t0
    var top = t0
    if (chance(0.5)) { top = genTpl("trait", "T1", Some(top)); tpls += top }
    if (chance(0.6)) { top = genTpl("abstract class", "AC", Some(top)); tpls += top }
    val leaves = List.tabulate(1 + r.nextInt(2))(i => genTpl("class", "C" + i, Some(if (chance(0.8)) top else pick(tpls))))
    tpls ++= leaves
    tpls.foreach(t => units += (imp :: header(t) :: t.lines.toList) :+ "}")
    // Main.run calls one small method per instance and per ancestor view, so no method grows with the program
    val fields = ArrayBuffer[String](); val meths = ArrayBuffer[ArrayBuffer[String]]()
    def newM(): ArrayBuffer[String] = { val b = ArrayBuffer[String](); meths += b; b }
    val env = Env(Nil, Nil, Set(), Set())
    var n = 0
    for (c <- leaves; _ <- 0 until (if (c.tps.isEmpty) 1 else 2)) {
      n += 1
      val s: Map[String, Ty] = c.tps.map(tp => tp.name -> (if (chance(0.65)) pick(List(TInt, TLong, TDouble)) else baseTy())).toMap
      val v = "c" + n
      val args = c.meths.filter(_.idx == -1).map(z => gen(subst(z.ret, s), env, 2))
      val targs = if (c.tps.isEmpty) "" else c.tps.map(tp => show(s(tp.name))).mkString("[", ", ", "]")
      fields += s"  var $v: ${c.name}$targs = null"
      val m0 = newM()
      m0 += s"    $v = new ${c.name}$targs(${args.mkString(", ")}); sb.append(\"$v [\" + Tr.take() + \"]\\n\")"
      c.meths.foreach(m => exercise(v, v, substM(m, s), m0))
      // the same object through each ancestor's type
      var anc = c.parent; var sub = s
      while (anc.nonEmpty) {
        val (p, pa) = anc.get
        val ps: Map[String, Ty] = p.tps.map(_.name).zip(pa.map(subst(_, sub))).toMap
        val w = v + p.name
        val mk = newM()
        mk += s"    val $w: ${p.name}${p.tps.map(tp => show(ps(tp.name))).mkString("[", ", ", "]")} = $v"
        p.meths.foreach(m => exercise(w, w, substM(m, ps), mk))
        anc = p.parent; sub = ps
      }
    }
    for (d <- vcs) {
      n += 1; val v = "w" + n
      val ty = TVC(d.name, d.tparam.toList.map(_ => pick(List(TInt, TLong, TDouble, TStr))))
      val mk = newM()
      mk += s"    val $v: ${show(ty)} = ${gen(ty, env, 1)}"
      val s: Map[String, Ty] = d.tparam.map(_ -> ty.targs.head).toMap
      vcMeths(d.name).foreach(m => exercise(v, v, substM(m, s), mk))
      mk += s"""    try { val r: Any = $v; sb.append("$v.any=" + r + (r == $v)) } catch { case e: Throwable => sb.append("$v.any!" + e.getClass.getName) }; sb.append('\\n')"""
    }
    val main = ArrayBuffer[String](imp, "object Main {") ++ fields
    meths.zipWithIndex.foreach { case (b, i) => main += s"  def r$i(sb: java.lang.StringBuilder): Unit = {"; main ++= b; main += "  }" }
    main += "  def run(): String = {"
    main += "    val sb = new java.lang.StringBuilder"
    main ++= meths.indices.map(i => s"    r$i(sb)")
    main ++= List("    sb.toString", "  }", "  def main(args: Array[String]): Unit = print(run())", "}")
    units += main.toList
    units.toList
  }
}

object Gen {
  /** Super calls to methods with their own type parameters hit two known faults (see the README); off unless -Dw4e.superpoly=true. */
  val specWeight: Double = sys.props.get("w4p.spec").fold(0.0)(_.toDouble)
  val superPoly: Boolean = sys.props.get("w4p.superpoly").contains("true")
  /** Up to `tries` attempts with derived seeds; returns the units and the number of attempts that the generator abandoned. */
  def make(seed: Long, opts: GenOpts = GenOpts()): (List[List[String]], Int) = {
    var k = 0
    while (true) {
      try return (new Gen(seed * 1000 + k, opts).program(), k)
      catch { case _: GenFail | _: StackOverflowError => k += 1 }
    }
    sys.error("unreachable")
  }
  def main(args: Array[String]): Unit = println(make(args(0).toLong)._1.map(_.mkString("\n")).mkString("\n//#unit\n"))
}
