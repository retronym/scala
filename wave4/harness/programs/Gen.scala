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
case class Env(vars: List[Var], meths: List[Meth], ctags: Set[String], tps: Set[String], inVC: Boolean = false, ret: Option[Ty] = None)
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
  /** Construct families by the round that added them; each program favours two of those switched on, so that
   *  constructs of different rounds meet in one program. */
  val families = List(2 -> "switch", 3 -> "struct", 4 -> "narrow", 5 -> "return", 6 -> "patmat", 7 -> "nested", 8 -> "linear", 9 -> "vcmix", 10 -> "dyn")
  val focus: Set[String] = r.shuffle(families.filter(_._1 <= Gen.round).map(_._2)).take(2).toSet
  def weight(f: String): Int = if (focus(f)) 4 else 1

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
    case v: TVC => erasureKey(under(v)) // a generic value class erases to its underlying type as instantiated
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
      nodes += (() => { val f = fresh("loc"); val e = env.copy(ret = Some(t)); s"({ def $f(): ${show(t)} = { try { if (${gen(TBool, e, d - 1)}) return ${gen(t, e, d - 1)}; ${gen(t, e, d - 1)} } finally { Tr.t(${fresh("")}, 0) } }; $f() })" })
      nodes += (() => { val (f, i, acc) = (fresh("loop"), fresh("i"), fresh("acc"))
        s"({ @scala.annotation.tailrec def $f($i: Int, $acc: ${show(t)}): ${show(t)} = if ($i <= 0) $acc else $f($i - 1, ${gen(t, w(Var(acc, t), Var(i, TInt)).copy(ret = Some(t)), d - 1)}); $f(2, ${g(t)}) })" })
      nodes += (() => { val x = fresh("x"); s"({ case $x if $x > 1 => ${gen(t, w(Var(x, TInt)), d - 1)} }: PartialFunction[Int, ${show(t)}]).applyOrElse(${g(TInt)}, (($x: Int) => ${g(t)}))" })
      env.vars.filter(_.mutable).foreach(v => nodes += (() => s"({ ${v.name} = ${g(v.ty)}; ${g(t)} })"))
      def addF(f: String)(th: () => String): Unit = for (_ <- 0 until weight(f)) nodes += th
      if (Gen.round >= 10) { // Dynamic, string interpolators (standard, user-defined, in patterns), for comprehensions with pattern generators over several collections
        def b(t2: Ty, vs: Var*) = gen(t2, w(vs: _*), d - 2)
        if (t == TStr) addF("dyn")(() => pick(List(s"new Dyn().foo", s"new Dyn().bar(${g(TInt)}, ${g(TStr)})", s"new Dyn().baz(x = ${g(TInt)}, ${g(TStr)})", s"{ val dy = new Dyn; dy.q = ${g(TInt)}; dy.fields.toString + dy.q2 }")))
        if (t == TStr) addF("dyn")(() => pick(List("f\"${" + g(TInt) + "}%03d-${" + g(TDouble) + "}%.2f-${" + g(TStr) + "}%s\"", "raw\"a\\n${" + g(TStr) + "}\"", "ip\"x${" + g(TInt) + "}y${" + g(TStr) + "}z\"", "ip\"${" + g(TLong) + "}\"")))
        addF("dyn")(() => { val (x, y) = (fresh("x"), fresh("y"))
          "(" + g(TStr) + " match { case s\"a$" + x + "\" => " + b(t, Var(x, TStr)) + "; case s\"$" + x + "-$" + y + "\" => " + b(t, Var(x, TStr), Var(y, TStr)) + "; case ip\"<$" + x + ">\" => " + b(t, Var(x, TStr)) + "; case _ => " + g(t) + " })" })
        addF("dyn")(() => { val (k, v, x, y, z) = (fresh("k"), fresh("v"), fresh("x"), fresh("y"), fresh("z"))
          val all = r.shuffle(List(
            (s"($k, $v) <- Map[Int, String](1 -> ${g(TStr)}, 2 -> ${g(TStr)}).toList", List(Var(k, TInt), Var(v, TStr))),
            (s"Some($x) <- List[Option[Int]](Option(${g(TInt)}), None, Some(3))", List(Var(x, TInt))),
            (s"$y <- Vector[Int](1, ${g(TInt)})", List(Var(y, TInt))),
            (s"$z <- Set[Long](${g(TLong)}, 2L).toList.sorted", List(Var(z, TLong)))))
          val used = all.take(2 + r.nextInt(3)); val bound = used.flatMap(_._2)
          val filt = used.head._2.find(_.ty == TInt).fold("")(vv => s" if ${vv.name} > ${g(TInt)}")
          s"(for (${used.head._1}$filt; ${used.tail.map(_._1).mkString("; ")}) yield ${b(t, bound: _*)}).headOption.getOrElse(${gen(t, env, d - 2)})" })
        addF("dyn")(() => { val (x, i, l) = (fresh("x"), fresh("i"), fresh("last"))
          s"({ val it = Iterator[Int](1, 2, ${g(TInt)}); var $l: ${show(t)} = ${gen(t, env, d - 2)}; for ($x <- it if $x != 2; $i <- 0 until $x by 2) $l = ${b(t, Var(x, TInt), Var(i, TInt), Var(l, t))}; $l })" })
      }
      if (Gen.round >= 9 && vcs.nonEmpty) { // value classes as scrutinee, capture, lazy val, by-name argument, array and Seq element; defaults, named arguments, varargs with sequences
        def b(t2: Ty, vs: Var*) = gen(t2, w(vs: _*), d - 2)
        def vt(): TVC = { val dd = pick(vcs); TVC(dd.name, dd.tparam.toList.map(_ => pick(List(TInt, TLong, TStr, TDouble)))) }
        addF("vcmix")(() => { val v = vt(); val x = fresh("x"); val vv = fresh("vv")
          s"(${g(v)} match { case ${v.name}($x) if ${gen(TBool, w(Var(x, under(v))), d - 1)} => ${b(t, Var(x, under(v)))}; case $vv => ${b(t, Var(vv, v))} })" })
        addF("vcmix")(() => { val v = vt(); val x = fresh("v"); s"({ val $x: ${show(v)} = ${g(v)}; ((i: Int) => ${b(t, Var(x, v))}).apply(1) })" })
        addF("vcmix")(() => { val v = vt(); val x = fresh("v"); s"({ lazy val $x: ${show(v)} = Tr.t(${fresh("")}, ${g(v)}); ${b(t, Var(x, v))} })" })
        addF("vcmix")(() => { val v = vt(); val x = fresh("v"); s"({ val $x: ${show(v)} = Bn(${g(v)}); Bn.twice(${b(t, Var(x, v))})._1 })" })
        addF("vcmix")(() => { val v = vt(); val a = fresh("arr"); s"({ val $a = Array[${show(v)}](${g(v)}, ${g(v)}); $a(1) = ${g(v)}; ${b(t, Var(a, TArr(v)))} })" })
        addF("vcmix")(() => { val v = vt(); val q = fresh("sq"); s"({ val $q: Seq[${show(v)}] = Seq(${g(v)}, ${g(v)}).reverse; ${b(t, Var(q, TSeq(v)))} })" })
        if (t == TStr) addF("vcmix")(() => pick(List(s"Df.f(c = Tr.t(${fresh("")}, ${g(TStr)}), a = Tr.t(${fresh("")}, ${g(TInt)}))()", s"Df.f(Tr.t(${fresh("")}, ${g(TInt)}))(d = ${g(TInt)})", s"Df.f(b = ${g(TInt)}, a = ${g(TInt)})()", s"{ val dc = new DfC(b = Tr.t(${fresh("")}, ${g(TStr)}), a = Tr.t(${fresh("")}, ${g(TInt)}))(); dc.b + dc.c }")))
        if (t == TInt) addF("vcmix")(() => pick(List(s"Va.sumi(${g(TList(TInt))}: _*)", s"Va.sumi(Array[Int](${g(TInt)}, 2).toIndexedSeq: _*)", s"Vs.f(${g(TInt)}, Vector(${g(TInt)}): _*)", s"Vs.f(${g(TInt)})", s"Vs.g(List(1, ${g(TInt)}): _*)")))
        if (t == TStr) addF("vcmix")(() => s"Va.cat(Seq(${g(TStr)}, ${g(TStr)}): _*)")
      }
      if (Gen.round >= 8 && !env.inVC) { // stackable traits, trait field initialisation order, super calls through the linearization, lazy vals in objects
        if (t == TStr) addF("linear")(() => { val ts = r.shuffle(List("SA", "SB", "SC", "SD")).take(2 + r.nextInt(3)); val o = s"(new Im with ${ts.mkString(" with ")})"
          pick(List(s"$o.f(${g(TInt)})", s"{ val q = $o; q.f(${g(TInt)}) + q.tag + q.f(1) }", s"($o: Bs).f(${g(TInt)})")) })
        if (t == TStr && chance(0.5)) addF("linear")(() => pick(List("(new Im with SC with SB).lc", "(new Im with SA with SC).ta", "LzO.y", "new GG().h", "(new GG: G1).h", "(new G3 with G2 {}).h")))
        if (t == TInt) addF("linear")(() => pick(List("(new FI2 with FI3 {}).c", "(new FI3 with FI2 {}).c", "(new FI2 {}).c", "LzO.x", "(new Im with SB with SA).lb", "(new Im with SD with SB).ld", s"{ val q = new Im with SD; q.vd = ${g(TInt)}; q.ld }")))
      }
      if (Gen.round >= 7 && !env.inVC) { // local and nested classes and objects capturing variables and each other (lambdalift, explicitouter, constructors)
        def b(t2: Ty, e: Env) = gen(t2, e, d - 2)
        addF("nested")(() => { val (c, l, k) = (fresh("c"), fresh("L"), fresh("l")); val e = w(Var(c, TInt, mutable = true))
          s"({ var $c: Int = ${g(TInt)}; class $l { val k: ${show(t)} = ${b(t, e.copy(ret = None))}; def inc(): Int = { $c += 1; $c } }; val $k = new $l; $k.inc(); ${b(t, w(Var(c, TInt), Var(k + ".k", t)))} })" })
        addF("nested")(() => { val (c, o) = (fresh("c"), fresh("LO")); val e = w(Var(c, TStr, mutable = true))
          s"({ var $c: String = ${g(TStr)}; object $o { def f(): String = { $c = $c + \"!\"; $c }; lazy val k: ${show(t)} = ${b(t, e.copy(ret = None))} }; $o.f(); val r = $o.k; $c = \"z\"; Tr.t(${fresh("")}, $o.f()); r })" })
        addF("nested")(() => { val (a, bb, n) = (fresh("A"), fresh("B"), fresh("n"))
          s"({ class $a { def f($n: Int): Int = if ($n <= 0) ${gen(TInt, env.copy(ret = None), d - 1)} else new $bb().g($n - 1) + 1 }; class $bb { def g($n: Int): Int = new $a().f($n) * 2 }; Tr.t(${fresh("")}, new $a().f(3)); ${gen(t, env, d - 2)} })" })
        addF("nested")(() => { val (acc, f, x) = (fresh("acc"), fresh("add"), fresh("x"))
          s"({ var $acc = 0; def $f($x: Int): Int = { $acc += $x; $acc }; List[Int](1, 2, ${g(TInt)}).map($f); Tr.t($acc, ${gen(t, env, d - 2)}) })" })
        addF("nested")(() => { val (c, x) = (fresh("c"), fresh("x")); val e = w(Var(c, TInt, mutable = true), Var(x, TInt))
          s"({ var $c: Int = ${g(TInt)}; val s = new Sam0[Int, ${show(t)}] { var hits = 0; def ap($x: Int): ${show(t)} = { hits += 1; $c += $x; ${b(t, e.copy(ret = Some(t)))} } }; s.ap(${g(TInt)}) })" })
        addF("nested")(() => { val (l, a, s1) = (fresh("L"), fresh("a"), fresh("s"))
          s"({ class $l(val $a: Int, val $s1: String) { def this($a: Int) = this($a, \"d\" + $a); def this() = this(${lit(TInt)}); def show = $s1 + $a }; Tr.t(${fresh("")}, new $l(${g(TInt)}).show + new $l().show); ${gen(t, env, d - 2)} })" })
        addF("nested")(() => { val (c, l, m) = (fresh("c"), fresh("L"), fresh("M")); val e = w(Var(c, TInt))
          s"({ val $c: Int = ${g(TInt)}; class $l { class $m { def f: ${show(t)} = ${b(t, e.copy(ret = Some(t)))}; def g = $c + h }; def h = $c * 2 }; val o = new $l; new o.$m().g; new o.$m().f })" })
      }
      if (Gen.round >= 6) { // pattern matching: nested case classes with binders, alternatives, extractors (Boolean, name-based, unapplySeq), type patterns, catch patterns
        def b(t2: Ty, vs: Var*) = gen(t2, w(vs: _*), d - 2)
        addF("patmat")(() => { val (p, a, s1, x, rest, q, xs) = (fresh("p"), fresh("a"), fresh("s"), fresh("x"), fresh("r"), fresh("q"), fresh("xs"))
          s"(P3(P2(${g(TInt)}, ${g(TOpt(TStr))}), ${g(TList(TInt))}) match { case P3($p @ P2($a, Some($s1)), $x :: $rest) if $a > $x => ${b(t, Var(a, TInt), Var(s1, TStr), Var(x, TInt), Var(rest, TList(TInt)))}; case P3(P2($a, None), Nil) => ${b(t, Var(a, TInt))}; case P3(P2(0 | 1, _), _ :: _) => ${gen(t, env, d - 2)}; case P3($q, $xs) => ${b(t, Var(xs, TList(TInt)))} })" })
        addF("patmat")(() => { val (k, n) = (fresh("k"), fresh("n"))
          s"(${g(TInt)} match { case Pos() if ${g(TBool)} => ${gen(t, env, d - 2)}; case NB($k) => ${b(t, Var(k, TInt))}; case NB2($k, $n) => ${b(t, Var(k, TInt), Var(n, TStr))}; case _ => ${gen(t, env, d - 2)} })" })
        addF("patmat")(() => { val (a, c, xs) = (fresh("a"), fresh("c"), fresh("xs"))
          s"(${g(TStr)} match { case Digits() => ${gen(t, env, d - 2)}; case Digits($a) => ${b(t, Var(a, TInt))}; case Digits($a, $c, $xs @ _*) => ${b(t, Var(a, TInt), Var(c, TInt), Var(xs, TSeq(TInt)))}; case _ => ${gen(t, env, d - 2)} })" })
        addF("patmat")(() => { val (a, c, tl, rs) = (fresh("a"), fresh("c"), fresh("tl"), fresh("rs"))
          s"(${g(TList(TInt))} match { case List(1, _) | List(_, 2) => ${gen(t, env, d - 2)}; case List($a, $c) => ${b(t, Var(a, TInt), Var(c, TInt))}; case $a :: $c :: $tl => ${b(t, Var(a, TInt), Var(tl, TList(TInt)))}; case Seq($a, $rs @ _*) => ${b(t, Var(a, TInt), Var(rs, TSeq(TInt)))}; case _ => ${gen(t, env, d - 2)} })" })
        addF("patmat")(() => { val o = other(); val (x, y, a, c) = (fresh("x"), fresh("y"), fresh("a"), fresh("c"))
          s"((${g(o)}: Any) match { case $x: List[_] => ${b(t, Var(x + ".length", TInt))}; case Some($y: Int) => ${b(t, Var(y, TInt))}; case ($a: Int, $c) => ${b(t, Var(a, TInt))}; case $x: Array[Int] => ${gen(t, env, d - 2)}; case Gen1($y: String) => ${b(t, Var(y, TStr))}; case $x: Sh with Product => ${gen(t, env, d - 2)}; case _ => ${gen(t, env, d - 2)} })" })
        addF("patmat")(() => { val o = other(); val (y) = (fresh("y"))
          s"((Gen1[Any](${g(o)}): Gen1[Any]) match { case Gen1($y: Int) if $y > 2 => ${b(t, Var(y, TInt))}; case Gen1(_: String) | Gen1(_: Long) => ${gen(t, env, d - 2)}; case Gen1(Some($y)) => ${gen(t, env, d - 2)}; case _ => ${gen(t, env, d - 2)} })" })
        addF("patmat")(() => { val (a, c, e, m) = (fresh("a"), fresh("c"), fresh("e"), fresh("m"))
          s"""({ val P2($a, $c) = P2(${g(TInt)}, ${g(TOpt(TStr))}); ${b(t, Var(a, TInt), Var(c, TOpt(TStr)))} })""" })
        addF("patmat")(() => { val (e, m) = (fresh("e"), fresh("m"))
          s"""(try { if (${g(TBool)}) throw new IllegalArgumentException(${g(TStr)}); ${gen(t, env, d - 2)} } catch { case $e @ (_: IllegalStateException | _: UnsupportedOperationException) => ${gen(t, env, d - 2)}; case ExMsg($m) if $m.length > 1 => ${b(t, Var(m, TStr))}; case $e: IllegalArgumentException => ${b(t, Var(e + ".getMessage", TStr))} })""" })
      }
      if (Gen.round >= 5 && env.ret.nonEmpty) { // return from the enclosing method: plain, from closures, by-name arguments, try/finally
        val rt = env.ret.get
        def rv() = g(rt)
        def cond() = g(TBool)
        addF("return")(() => s"({ if (${cond()}) return ${rv()}; ${g(t)} })")
        addF("return")(() => { val x = fresh("x"); s"({ List[Int](1, 2, 3).foreach { ($x: Int) => if ($x > ${g(TInt)}) return ${gen(rt, w(Var(x, TInt)), d - 1)} }; ${g(t)} })" })
        addF("return")(() => { val o = other(); val x = fresh("x"); s"Option[${show(o)}](${g(o)}).map((($x: ${show(o)}) => { if (${gen(TBool, w(Var(x, o)), d - 1)}) return ${rv()}; ${gen(t, w(Var(x, o)), d - 1)} })).getOrElse(${g(t)})" })
        addF("return")(() => s"Bn[${show(t)}](if (${cond()}) return ${rv()} else ${g(t)})")
        addF("return")(() => s"Bn.twice[${show(t)}](if (${cond()}) ${g(t)} else return ${rv()})._2")
        addF("return")(() => { val x = fresh("x"); s"(try { List[Int](1, 2).map(($x: Int) => if ($x == ${g(TInt)}) return ${rv()} else $x); ${g(t)} } finally { Tr.t(${fresh("")}, 0) })" })
        addF("return")(() => { val x = fresh("x"); s"(try { ${g(t)} } finally { List[Int](1).foreach(($x: Int) => Tr.t(${fresh("")}, $x)) })" })
        addF("return")(() => { val (i, x) = (fresh("i"), fresh("x")); s"({ var $i = 0; while ($i < 3) { if ($i == ${g(TInt)}) return ${rv()}; $i += 1 }; ${g(t)} })" })
        addF("return")(() => { val x = fresh("x"); s"({ case $x if $x > 1 => if ($x > ${g(TInt)}) return ${rv()} else ${gen(t, w(Var(x, TInt)), d - 1)} }: PartialFunction[Int, ${show(t)}]).applyOrElse(${g(TInt)}, (($x: Int) => ${g(t)}))" })
        addF("return")(() => { val x = fresh("x"); s"({ lazy val $x: ${show(t)} = { if (${cond()}) return ${rv()}; ${g(t)} }; $x })" })
        addF("return")(() => { val x = fresh("x"); s"(${g(TList(TInt))}.foldLeft[${show(t)}](${g(t)})((($x: ${show(t)}, y: Int) => if (y > 5) return ${rv()} else $x)))" })
      }
      if (Gen.round >= 4) { // Byte, Short, Char and Float operands, ## and getClass
        def narrow(): String = pick(List(s"${g(TInt)}.toByte", s"${g(TInt)}.toShort", s"(${g(TInt)} + 'a').toChar", s"${g(TInt)}.toFloat"))
        def narrowI(): String = pick(List(s"${g(TInt)}.toByte", s"${g(TInt)}.toShort", s"(${g(TInt)} + 'a').toChar"))
        if (t == TInt) addF("narrow")(() => s"(${narrow()} ${pick(List("+", "*", "-"))} ${narrow()}).toInt")
        if (t == TInt) addF("narrow")(() => s"(${narrowI()} ${pick(List("&", "<<", "|", "-"))} ${narrowI()})")
        if (t == TInt) addF("narrow")(() => s"(${g(TInt)}.toByte.##)")
        if (t == TInt) addF("narrow")(() => s"((${g(TInt)}.toShort: Any).##)")
        if (t == TLong) addF("narrow")(() => s"(${g(TLong)} ${pick(List("+", "&", "|", "^", "max"))} ${narrowI()})")
        if (t == TDouble) addF("narrow")(() => s"(${narrow()} / ${pick(List("3", "2.0f", "'b'", "7L"))})")
        if (t == TBool) addF("narrow")(() => s"(${narrow()} ${pick(List("<", "==", "!=", ">="))} ${narrow()})")
        if (t == TBool) addF("narrow")(() => s"(${narrow()}: Any).isInstanceOf[${pick(List("Byte", "Short", "Char", "Float", "Int"))}]")
        if (t == TStr) addF("narrow")(() => s"(${narrow()}: Any).getClass.getName")
        if (t == TStr) addF("narrow")(() => s"(${g(TStr)} + ${narrow()} + ${pick(List("'c'", "1.5f", "null", "2.toByte"))})")
      }
      if (Gen.round >= 3) { // structural (reflective) calls and @varargs members
        // a structural result type may not be a value class (SIP-15); type parameters are left out as they may stand for one
        val structOk = !t.isInstanceOf[TVC] && !t.isInstanceOf[TP]
        if (structOk) addF("struct")(() => s"({ val s: { def get(n: Int): ${show(t)} } = new Box[${show(t)}](${g(t)}); s.get(${g(TInt)}) })")
        if (structOk && !env.inVC) addF("struct")(() => { val x = fresh("x"); s"(new { def k($x: Int): ${show(t)} = ${gen(t, w(Var(x, TInt)).copy(ret = Some(t)), d - 1)} }).k(${g(TInt)})" })
        if (t == TLong) addF("struct")(() => s"({ val s: { def sum(a: Int, b: Long): Long } = new Box(0); s.sum(${g(TInt)}, ${g(TLong)}) })")
        if (t == TStr) addF("struct")(() => s"({ val s: { def cat(s: String, d: Double): String } = new Box(1); s.cat(${g(TStr)}, ${g(TDouble)}) })")
        if (t == TStr) addF("struct")(() => s"Va.cat(${List.fill(r.nextInt(3))(g(TStr)).mkString(", ")})")
        if (t == TInt) addF("struct")(() => s"Va.sumi(${List.fill(r.nextInt(3))(g(TInt)).mkString(", ")})")
      }
      if (Gen.round >= 2) { // switches over Int, Char and String literals
        def arm(pat: String) = if (chance(0.2)) s"case $pat if ${g(TBool)} => ${g(t)}" else s"case $pat => ${g(t)}"
        def ann = if (chance(0.3)) ": @scala.annotation.switch" else ""
        addF("switch")(() => { val x = fresh("x"); val ks = r.shuffle(List(0, 1, 2, 3, 5, 7, -1, 100)).take(3 + r.nextInt(4))
          val cs = ks.map(k => arm(if (chance(0.25)) s"$k | ${k + 1000}" else k.toString))
          s"((${g(TInt)}$ann) match { ${cs.mkString("; ")}; case $x => ${gen(t, w(Var(x, TInt)), d - 1)} })" })
        addF("switch")(() => { val x = fresh("x"); val ks = r.shuffle(List("a", "bc", "", "xyz", "Aa", "BB", "abc")).take(3 + r.nextInt(4)).map("\"" + _ + "\"")
          val scr = if (chance(0.3)) s"""(if (${g(TBool)}) "Aa" else "BB")""" else g(TStr)
          val cs = ks.grouped(1 + r.nextInt(2)).map(k => arm(k.mkString(" | "))).toList
          s"(($scr$ann) match { ${cs.mkString("; ")}; case $x => ${gen(t, w(Var(x, TStr)), d - 1)} })" })
        addF("switch")(() => { val ks = r.shuffle(List('a', 'A', 'b', 'B', 'z')).take(3 + r.nextInt(2)).map("'" + _ + "'")
          val cs = ks.grouped(1 + r.nextInt(2)).map(k => arm(k.mkString(" | "))).toList
          s"""(("aAbBz".charAt(Math.abs(${g(TInt)}) % 5)$ann) match { ${cs.mkString("; ")}; case _ => ${g(t)} })""" })
      }
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
          val benv = env.copy(vars = ps.map(p => Var(p.name, p.ty)) ::: env.vars, ret = Some(ret))
          sb ++= s"def $f(${ps.map(showParam).mkString(", ")}): ${show(ret)} = ${gen(ret, benv, d - 1)}; "
          env = env.copy(meths = Meth(0, f, "def", Nil, ps, ret, render = Some(as => s"$f(${as.mkString(", ")})")) :: env.meths)
        case 5 =>
          val c = fresh("L"); val ret = genTy(env, 1, false)
          val p = Param(fresh("p"), genTy(env, 1, false)); val q = Param(fresh("p"), genTy(env, 1, false))
          val benv = env.copy(vars = Var(p.name, p.ty) :: Var(q.name, q.ty) :: env.vars, ret = Some(ret))
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
    if (ps.nonEmpty && chance(0.2)) { val l = ps.last; if (!l.byName && tpsOf(l.ty).isEmpty && showable(l.ty)) ps = ps.init :+ l.copy(default = Some(gen(l.ty, Env(Nil, Nil, Set(), Set(), inVC = env.inVC), 1))) }
    else if (chance(0.2)) ps = ps :+ Param(fresh("p"), genTy(env2, 1, true), vararg = true)
    Meth(idx, if (name == null) fresh("m") else name, "def", tps, ps, retTy(env2))
  }
  def bodyEnv(m: Meth, env: Env): Env = env.copy(
    vars = m.params.map(p => Var(p.name, if (p.vararg) TSeq(p.ty) else p.ty)) ::: env.vars,
    meths = env.meths.filter(_.idx < m.idx), tps = env.tps ++ m.tps.map(_.name), ctags = env.ctags ++ m.tps.filter(_.ctag).map(_.name), ret = Some(m.ret))

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
          val ann = if (Gen.round >= 8) pick(List("", "", "@volatile ", "@transient ", "private[this] ".take(0))) else ""
          t.lines += s"  $ann${sig(m)} = ${gen(ty, env, 3)}"
          all = all :+ m
        case 5 if Gen.round >= 7 && chance(0.5) => // a member class using the outer instance's members
          val ty = retTy(env); val c = fresh("In"); val m = Meth(idx(), fresh("m"), "def", Nil, Nil, ty); val x = fresh("x")
          val e = env.copy(meths = env.meths.filter(_.idx < m.idx), vars = List(Var(x, TInt)), ret = Some(ty))
          t.lines += s"  class $c($x: Int) { def get: ${show(ty)} = ${gen(ty, e, 2)} }; ${sig(m)} = new $c(${gen(TInt, Env(Nil, Nil, Set(), Set()), 1)}).get"
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
            // numeric types share a key: Int against Double in a subclass override is ambiguous by the overloading rules
            val key = ps.map(p => erasureKey(p.ty)).mkString.replaceAll("[IJD]", "N")
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
    val imp = "import scala.reflect.ClassTag; import Imp._" + (if (Gen.round >= 3) "; import scala.language.reflectiveCalls" else "") + (if (Gen.round >= 10) "; import scala.language.dynamics; import Ipx._" else "")
    units += List(imp0, "trait U0[A] extends Any { def uu: A; def uc(x: Int): (A, Int) = (uu, x) }")
    units += List(imp0, s"trait Sam0[${specAnn()}A, ${specAnn()}B] { def ap(a: A): B${if (chance(0.3)) "; def twice(a: A): (B, B) = (ap(a), ap(a))" else ""} }")
    units += List(imp0,
      "object Tr { val sb = new java.lang.StringBuilder; def t[T](k: Int, v: T): T = { sb.append(k).append(' '); v }; def take(): String = { val s = sb.toString; sb.setLength(0); s } }",
      "sealed trait Sh",
      "case class K0(a: Int, b: String) extends Sh",
      "case class K1(x: Sh, n: Long) extends Sh",
      "case object K2 extends Sh",
      "class Box[A](val v: A) { def get(n: Int): A = v; def sum(a: Int, b: Long): Long = a + b * 2; def cat(s: String, d: Double): String = s + d }",
      "object Va { @scala.annotation.varargs def cat(xs: String*): String = xs.mkString(\"/\"); @scala.annotation.varargs def sumi(xs: Int*): Int = xs.sum }",
      "object Ev { def unapply(i: Int): Option[Int] = if (i % 2 == 0) Some(i / 2) else None }",
      "object Bn { def apply[T](x: => T): T = x; def twice[T](x: => T): (T, T) = (x, x) }",
      "case class P2(a: Int, b: Option[String]); case class P3(p: P2, xs: List[Int]); case class Gen1[T](x: T)",
      "object Pos { def unapply(i: Int): Boolean = i > 0 }; object NB { def unapply(i: Int): NBR = new NBR(i) }; class NBR(val i: Int) { def isEmpty: Boolean = i % 3 == 0; def get: Int = i * 2 }",
      "object NB2 { def unapply(i: Int): Option[(Int, String)] = if (i < 0) Some((i, \"neg\")) else None }; object ExMsg { def unapply(e: Throwable): Option[String] = Option(e.getMessage) }",
      "abstract class Bs { def f(x: Int): String; val tag: String = Tr.t(901, \"bs\") }; class Im extends Bs { def f(x: Int): String = \"i\" + x }",
      "trait SA extends Bs { abstract override def f(x: Int): String = \"a(\" + super.f(x + 1) + \")\"; val ta: String = Tr.t(902, \"A\" + tag) }",
      "trait SB extends Bs { abstract override def f(x: Int): String = super.f(x * 2) + \"b\"; lazy val lb: Int = Tr.t(903, f(1).length) }",
      "trait SC extends SA { abstract override def f(x: Int): String = \"c\" + super.f(x) + ta; @volatile lazy val lc: String = Tr.t(904, \"C\" + ta) }",
      "trait SD extends Bs { abstract override def f(x: Int): String = super.f(x - 1) + vd; var vd: Int = Tr.t(905, 4); @transient lazy val ld: Int = vd + 1 }",
      "trait FI1 { val a: Int = Tr.t(910, 1); val b: Int = a + 1 }; trait FI2 extends FI1 { val c: Int = b * 10 + a }; trait FI3 extends FI1 { override val a: Int = Tr.t(911, 5) }",
      "object LzO { @volatile lazy val x: Int = Tr.t(920, 3); @transient lazy val y: String = Tr.t(921, \"y\" + x) }",
      "trait G1 { def h: String = \"g1\" }; trait G2 extends G1 { override def h: String = \"g2\" + super.h }; trait G3 extends G1 { override def h: String = \"g3\" + super.h }; class GG extends G2 with G3 { override def h: String = super[G2].h + super[G3].h + super.h }",
      "object Df { def f(a: Int, b: Int = Tr.t(930, 1) + 1, c: String = \"c\")(d: Int = b * 2): String = a + \",\" + b + \",\" + c + \",\" + d }; class DfC(val a: Int, val b: String = \"x\")(val c: Int = a * 2)",
      "object Vs { def f(a: Int, xs: Int*): Int = a * 10 + xs.sum; def g(xs: Int*): Int = xs.length }",
      "import scala.language.dynamics; class Dyn extends scala.Dynamic { def selectDynamic(n: String): String = \"sel:\" + n; def applyDynamic(n: String)(args: Any*): String = n + args.mkString(\"(\", \",\", \")\"); def applyDynamicNamed(n: String)(args: (String, Any)*): String = n + args.mkString; var fields = Map.empty[String, Any]; def updateDynamic(n: String)(v: Any): Unit = fields += n -> v }",
      "object Ipx { implicit class Ip(private val sc: StringContext) extends AnyVal { def ip(args: Any*): String = sc.parts.zipAll(args, \"\", \"\").map { case (p, a) => p + \"[\" + a + \"]\" }.mkString; def ip: IpPat = new IpPat(sc) } }",
      "class IpPat(sc: StringContext) { def unapplySeq(s: String): Option[Seq[String]] = if (sc.parts.length == 2 && s.startsWith(sc.parts(0)) && s.endsWith(sc.parts(1)) && s.length >= sc.parts(0).length + sc.parts(1).length) Some(Seq(s.substring(sc.parts(0).length, s.length - sc.parts(1).length))) else None }",
      "object Digits { def unapplySeq(s: String): Option[Seq[Int]] = if (s.forall(_.isLetter)) Some(s.map(_.toInt).toSeq) else None }",
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
      // at most 12 calls per method: -opt:local's cost grows steeply with the exception handlers in a method (GE6)
      c.meths.grouped(12).zipWithIndex.foreach { case (ms, i) => val b = if (i == 0) m0 else newM(); ms.foreach(m => exercise(v, v, substM(m, s), b)) }
      // the same object through each ancestor's type
      var anc = c.parent; var sub = s
      while (anc.nonEmpty) {
        val (p, pa) = anc.get
        val ps: Map[String, Ty] = p.tps.map(_.name).zip(pa.map(subst(_, sub))).toMap
        val w = v + p.name
        p.meths.grouped(12).foreach { ms =>
          val mk = newM()
          mk += s"    val $w: ${p.name}${p.tps.map(tp => show(ps(tp.name))).mkString("[", ", ", "]")} = $v"
          ms.foreach(m => exercise(w, w, substM(m, ps), mk))
        }
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
  /** Steering round of the campaign; each round switches on the constructs added for it. */
  val round: Int = sys.props.get("w4p.round").fold(1)(_.toInt)
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
