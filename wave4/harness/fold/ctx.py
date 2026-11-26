#!/usr/bin/env python3
"""Embedding-context campaign on top of fold.py.

A tree from fold.gen, with some leaves traced, is placed in a hole of a context template (match
scrutinee, guard, try/finally, while, return in closures, constructor and super arguments, field
initialisers, structural calls, Dynamic, interpolators, long concatenations, ...). The hole is
filled in four spellings that must give the same value and the same trace:

  i  the expression inline, ascribed to its type
  v  { val v_ : T = <expr>; v_ }
  m  a call to an @noinline method in another object whose body is the expression
  f  a closure literal applied at once

each compiled without and with -opt:inline:** -opt:local, so a case has up to eight values.
"""
import argparse, os, random, re, subprocess, sys, shutil, json, time
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import fold
from fold import N, TNAME, NUM

fold.EVOLVED = True
SPELL = 'ivmf'
ZERO = dict(fold.ZERO, T='"z"')
JACOCO = None
BUILD = 'int'
BUILDS = dict(fold.BUILDS, w4='/Users/jzaugg-virtus/code/.worktrees/scala/w4-int/build/pack')

# ---------------------------------------------------------------- contexts
# (name, family, types or None for all, decls, expr). Placeholders: $E hole, $T type, $Z zero of $T,
# $P1 $P2 literal patterns of $T, $R type of $T + Int. Decls live in the case's own object.
ALL = None
NUMT = ['B', 'S', 'C', 'I', 'J', 'F', 'D']
ARITHT = ['I', 'J', 'F', 'D']

DYN = ('class Dy extends scala.Dynamic { var last: Any = null; def selectDynamic(n: String): Any = { mk(5); n }; '
       'def applyDynamic(n: String)(as: Any*): String = n + as.mkString("(", ",", ")"); '
       'def applyDynamicNamed(n: String)(as: (String, Any)*): String = n + as.mkString("[", ",", "]"); '
       'def updateDynamic(n: String)(v: Any): Unit = { last = v } }')
CONTEXTS = [
    # ---- control flow
    ('scrut', 'ctl', ['I', 'C', 'J', 'T', 'Z', 'B', 'S'], '',
     '(($E) match { case $P1 => mk(1); "a"; case $P2 => "b"; case x => "c" + x })'),
    ('scrutf', 'ctl', ['F', 'D'], '',
     '(($E) match { case x if x > 0 => mk(1); "p" + x; case x => "n" + x })'),
    ('scrutany', 'ctl', ALL, '',
     '((($E): Any) match { case x: Int => "I" + x; case x: Long => "J" + x; case s: String => "T" + s; case x => "o" + x })'),
    ('guard', 'ctl', ALL, '',
     '{ var g_ : $T = $Z; mk(0) match { case 0 if { g_ = $E; mk(1); g_ == g_ } => "g" + g_; case _ => "n" + g_ } }'),
    ('guard2', 'ctl', ['Z'], '',
     '(mk(0) match { case 1 => "one"; case 0 if $E => "g"; case _ => "n" })'),
    ('casebody', 'ctl', ALL, '', '(mk(1) match { case 1 => $E; case _ => $Z })'),
    ('casebody2', 'ctl', ALL, '', '((mk(2): Any) match { case 1 => $Z; case x: Int if x > 1 => mk(3); $E; case _ => $Z })'),
    ('tryblk', 'ctl', ALL, '', '(try $E finally mk(1))'),
    ('trycatch', 'ctl', ALL, '', '(try { mk(1); $E } catch { case e: ArithmeticException => mk(2); $Z })'),
    ('handler', 'ctl', ALL, '',
     '(try { mk(1); throw new RuntimeException("x") } catch { case _: IllegalStateException => $Z; case _: RuntimeException => $E })'),
    ('handler2', 'ctl', ALL, 'class X2 extends Exception',
     '(try { if (mk(1) == 1) throw new X2; $Z } catch { case e: X2 if mk(2) == 2 => $E } finally mk(3))'),
    ('finalizer', 'ctl', ALL, '', '{ var r_ : $T = $Z; try { mk(1) } finally { r_ = $E }; r_ }'),
    ('finret', 'ctl', ALL, 'def f(): $T = try { mk(1); return $Z } finally { mk(2); return $E }', 'f()'),
    ('whilecond', 'ctl', ALL, '', '{ var i_ = 0; var r_ : $T = $Z; while ({ i_ += 1; r_ = $E; i_ < 3 }) mk(i_); r_ }'),
    ('whilez', 'ctl', ['Z'], '', '{ var i_ = 0; while ($E && i_ < 2) i_ += 1; i_ }'),
    ('whilebody', 'ctl', ALL, '', '{ var i_ = 0; var r_ : $T = $Z; while (i_ < 2) { i_ += 1; r_ = $E }; r_ }'),
    ('dowhile', 'ctl', ALL, '', '{ var i_ = 0; var r_ : $T = $Z; do { i_ += 1; r_ = $E } while (i_ < 2); r_ }'),
    ('dowhilez', 'ctl', ['Z'], '', '{ var i_ = 0; do { i_ += 1 } while ($E && i_ < 3); i_ }'),
    ('ret', 'ctl', ALL, 'def f(): $T = { mk(1); return $E }', 'f()'),
    ('retif', 'ctl', ALL, 'def f(x: Int): $T = { if (x > 0) return $E; mk(2); $Z }', 'f(mk(1))'),
    ('retclos', 'ctl', ALL, 'def f(): $T = { List(1, 2).foreach { k => mk(k); if (k == 2) return $E }; $Z }', 'f()'),
    ('retnest', 'ctl', ALL,
     'def f(): $T = { try { (1 to 2).foreach { k => Option(k).foreach { j => try { if (j == 2) return $E } finally mk(10 + j) } } } finally mk(3); $Z }',
     'f()'),
    ('rettry', 'ctl', ALL,
     'def f(): $T = try { mk(1); return $E } catch { case _: IllegalStateException => mk(2); $Z } finally mk(3)', 'f()'),
    ('retmatch', 'ctl', ALL, 'def f(x: Int): $T = { try { x match { case 1 => return $E; case _ => } } finally mk(4); $Z }',
     'f(1)'),
    ('retunit', 'ctl', ALL, 'def f(): Unit = { mk(1); return $E }', '{ f(); "u" }'),
    ('retsync', 'ctl', ALL, 'def f(): $T = this.synchronized { if (mk(1) == 1) return $E; $Z }', 'f()'),
    ('throw', 'ctl', ALL, 'class X(val x: Any) extends RuntimeException',
     '(try throw new X($E) catch { case e: X => e.x })'),
    ('throwif', 'ctl', ALL, 'class X(val x: Any) extends RuntimeException',
     '(try { mk(1); throw (if (mk(2) > 0) new X($E) else null) } catch { case e: X => e.x })'),
    ('byname', 'ctl', ALL, 'def f[A](a: => A): A = { mk(1); val x = a; mk(2); a }', 'f($E)'),
    ('byname2', 'ctl', ALL, 'def f(a: => $T): String = { mk(1); "" + a + a }', 'f($E)'),
    ('byname3', 'ctl', ALL, 'def f(c: Boolean)(a: => $T): Any = if (c) a else "no"', '(f(mk(0) == 0)($E), f(false)($E))'),
    ('sync', 'ctl', ALL, '', 'this.synchronized { mk(1); $E }'),
    ('sync2', 'ctl', ALL, '', '{ val l = new Object; mk(1); l.synchronized($E) }'),
    ('asgvar', 'ctl', ALL, '', '{ var r_ : $T = $Z; mk(1); r_ = $E; r_ }'),
    ('asgfield', 'ctl', ALL,
     'class H { var f: $T = $Z; private[this] var g_ : $T = $Z; def g: $T = g_; def g_=(x: $T): Unit = { mk(7); g_ = x } }',
     '{ val h = new H; h.f = $E; h.g = $E; "" + h.f + "," + h.g }'),
    ('asgarr', 'ctl', ALL, '', '{ val a = new Array[$T](2); a(mk(1)) = $E; a(1) }'),
    ('asgop', 'ctl', ARITHT + ['T'], '', '{ val a = Array.fill[$T](2)($Z); a(mk(1)) += $E; a(mk(1)) += $E; a(1) }'),
    ('asgopf', 'ctl', ARITHT + ['T'], 'class H { private[this] var g_ : $T = $Z; def g: $T = { mk(6); g_ }; def g_=(x: $T): Unit = { mk(7); g_ = x } }',
     '{ val h = new H; h.g += $E; h.g }'),
    ('asgopv', 'ctl', ARITHT, '', '{ var r_ : $T = $Z; r_ *= $E; r_ -= $E; r_ }'),
    ('locallazy', 'ctl', ALL, '', '{ mk(1); lazy val l_ : $T = $E; mk(2); "" + l_ + l_ }'),
    ('tuplepat', 'ctl', ALL, '', '{ val (a_, b_) = ($E, mk(2)); "" + a_ + b_ }'),
    ('tuplepat2', 'ctl', ALL, '', '{ val (a_, b_ : $T) = (mk(1), $E); "" + a_ + b_ }'),
    ('somepat', 'ctl', ALL, '', '{ val Some(x_) = Option($E); x_ }'),
    ('listpat', 'ctl', ALL, '', '{ val a_ :: b_ :: Nil = List[Any](mk(1), $E); b_ }'),
    # ---- definitions
    ('dflt', 'def', ALL, 'def f(a: Int = mk(1), p: $T = $E, q: Int = mk(2)): String = "" + a + p + q', 'f()'),
    ('dfltnamed', 'def', ALL, 'def f(a: Int = mk(1), p: $T = $E, q: Int = mk(2)): String = "" + a + p + q', 'f(q = mk(5), a = mk(6))'),
    ('dfltnamed2', 'def', ALL, 'def f(a: Int, p: $T, q: Int): String = "" + a + p + q', 'f(q = mk(5), p = $E, a = mk(6))'),
    ('dfltctor', 'def', ALL, 'class C(val a: Int = mk(1), val p: $T = $E)', 'new C(a = mk(3)).p'),
    ('dfltcurry', 'def', ALL, 'def f(a: Int)(p: $T = $E): String = "" + a + p', 'f(mk(4))()'),
    ('ctorarg', 'def', ALL, 'class C(val p: $T)', 'new C($E).p'),
    ('ctorarg2', 'def', ALL, 'class C(p: $T) { def g = p }', 'new C($E).g'),
    ('ctorcase', 'def', ALL, 'case class C(p: $T, q: Int)', 'C(q = mk(2), p = $E).toString'),
    ('superarg', 'def', ALL, 'class P(val p: $T); class S extends P({ mk(1); $E })', 'new S().p'),
    ('superarg2', 'def', ALL, 'class P(val p: $T, val q: Int); class S(x: Int) extends P(if (x > 0) $E else $Z, mk(x))',
     'new S(mk(2)).p'),
    ('superarg3', 'def', ALL, 'class P(val p: Any); class S(c: Int) extends P(() => $E) { val g = p.asInstanceOf[() => Any]() }',
     'new S(1).g'),
    ('superauxarg', 'def', ALL, 'class P(val p: $T) { def this() = this($E) }; class S extends P', 'new S().p'),
    ('classfield', 'def', ALL,
     'class F { val a = mk(1); val f: $T = $E; var g: $T = $E; lazy val l: $T = $E; mk(2) }',
     '{ val o = new F; mk(9); "" + o.f + o.g + o.l + o.l }'),
    ('privfield', 'def', ALL, 'class F { private[this] val f: $T = $E; def get = f }', 'new F().get'),
    ('ctorparamfield', 'def', ALL, 'class F(p: $T) { val q: Any = p; def r = () => p }', '{ val o = new F($E); "" + o.q + o.r() }'),
    ('objfield', 'def', ALL, 'object OF { mk(1); val f: $T = $E; mk(2) }', 'OF.f'),
    ('objlazy', 'def', ALL, 'object OF { lazy val f: $T = $E }', '{ mk(1); "" + OF.f + OF.f }'),
    ('traitfield', 'def', ALL, 'trait Tr { mk(1); val f: $T = $E; mk(2) }; class TC extends Tr', 'new TC().f'),
    ('traitvar', 'def', ALL, 'trait Tr { var f: $T = $E }; class TC extends Tr', '{ val o = new TC; o.f = $E; o.f }'),
    ('traitlazy', 'def', ALL, 'trait Tr { lazy val f: $T = $E }; class TC extends Tr', '{ val o = new TC; "" + o.f + o.f }'),
    ('traitobj', 'def', ALL, 'trait Tr { val f: $T = $E }; object TO extends Tr', 'TO.f'),
    ('traitdef', 'def', ALL, 'trait Tr { def f: $T = $E }; class TC extends Tr', 'new TC().f'),
    ('early', 'def', ALL, 'trait ET { val e: $T; val g: String = "" + e }; class EC extends { val e: $T = $E } with ET',
     'new EC().g'),
    ('lazyclass', 'def', ALL, 'class L { lazy val l: $T = { mk(1); $E } }', '{ val o = new L; mk(2); "" + o.l + o.l }'),
    ('localclass', 'def', ALL, '', '{ class LC { val f: $T = $E }; new LC().f }'),
    ('anonclass', 'def', ALL, 'abstract class A { def f: $T }', '{ val p: $T = $Z; new A { val f: $T = $E }.f }'),
    ('vcarg', 'def', ALL, 'class VC(val x: $T) extends AnyVal { def g: Any = x }', 'new VC($E).g'),
    # ---- calls, strings, arrays
    ('struct', 'dyn', ALL, '', '{ val s: { def m(x: $T): $T } = new { def m(x: $T): $T = { mk(1); x } }; s.m($E) }'),
    ('structrecv', 'dyn', ALL, 'class K { def m(x: Int): Int = x + 1 }',
     '{ (if (mk(1) > 0) new K else null: { def m(x: Int): Int }).m(mk(2)) + ($E).toString }'),
    ('structstr', 'dyn', ALL, '', '(($E): Any { def toString(): String }).toString'),
    ('structhash', 'dyn', ALL, '', '(($E): Any { def hashCode(): Int }).hashCode'),
    ('structeq', 'dyn', ALL, '', '(($E): Any { def equals(x: Any): Boolean }).equals($E)'),
    ('structtolong', 'dyn', NUMT, '', '(($E): Any { def toLong: Long }).toLong'),
    ('structplus', 'dyn', NUMT, '', '(($E): Any { def +(x: Int): $R }).+(mk(1))'),
    ('structneg', 'dyn', ARITHT, '', '(($E): Any { def unary_- : $T }).unary_-'),
    ('structlt', 'dyn', NUMT, '', '(($E): Any { def <(x: Int): Boolean }).<(mk(3))'),
    ('structmod', 'dyn', ['I', 'J'], '', '(($E): Any { def %(x: Int): $T }).%(mk(3))'),
    ('structz', 'dyn', ['Z'], '', '{ val s: Any { def unary_! : Boolean; def &(x: Boolean): Boolean } = $E; (s.unary_!, s & true) }'),
    ('structs', 'dyn', ['T'], '', '{ val s: Any { def length(): Int; def charAt(i: Int): Char } = $E; s.length() }'),
    ('dynapply', 'dyn', ALL, DYN,
     'new Dy().foo($E, mk(2))'),
]
CONTEXTS += [
    ('dynnamed', 'dyn', ALL, DYN, 'new Dy().foo(a = $E, b = mk(2))'),
    ('dynupd', 'dyn', ALL, DYN, '{ val d = new Dy; d.bar = $E; d.last }'),
    ('dynrecv', 'dyn', ALL, DYN, '{ mk(1); new Dy }.baz(mk(2), $E)'),
    ('interp', 'dyn', ALL, '', 's"<${mk(1)}|${$E}|${mk(2)}>"'),
    ('interpf', 'dyn', ALL, '', 'f"${$E}%s|${mk(1)}%d"'),
    ('interpfd', 'dyn', ['B', 'S', 'I', 'J'], '', 'f"${$E}%d|${$E}%x"'),
    ('interpraw', 'dyn', ALL, '', 'raw"\\n${$E}"'),
    ('varargs', 'dyn', ALL, '', 'List[$T]($Z, $E, $Z).apply(1)'),
    ('varargs2', 'dyn', NUMT + ['T'], 'def f(xs: $T*): String = xs.mkString(",")', 'f(mk(1).to$T2, $E)'),
    ('varargs3', 'dyn', ALL, '', 'java.util.Arrays.asList[Any](mk(1), $E).toString'),
    ('varargs4', 'dyn', ALL, '', 'String.format("%s|%s", $E.asInstanceOf[AnyRef], mk(2).asInstanceOf[AnyRef])'),
    ('arrelem', 'dyn', ALL, '', '{ val a = Array[$T]($Z, $E); a(1) }'),
    ('arrelem2', 'dyn', ALL, '', '{ val a = Array($E, $E); a(0) }'),
    ('arrelemany', 'dyn', ALL, '', '{ val a = Array[Any]($E, mk(1)); a(0) }'),
    ('arridx', 'dyn', NUMT, '', '{ val a = Array.tabulate(4)(i => i * 10); a(($E).toInt & 3) }'),
    ('concat', 'dyn', ALL, 'class Mu { var n = 0; override def toString = "m" + n }',
     '{ val m = new Mu; "" + m + { m.n += 1; $E } + m + () + null + WI(3) + { m.n += 1; () } + m }'),
    ('concatlong', 'dyn', ALL, 'class Mu { var n = 0; override def toString = "m" + n }', 'CONCATLONG'),
]
CTX = {c[0]: c for c in CONTEXTS}


# Expected trace of each context, from the specification: E stands for the trace of the expression
# evaluated alone, mK for a marker. None when the trace depends on the value. Checked only when the
# expression alone does not throw.
EXPECT = dict(
    scrutany='E', guard='m0,E,m1', guard2='m0,E', casebody='m1,E', casebody2='m2,m3,E', tryblk='E,m1',
    trycatch='m1,E', handler='m1,E', handler2='m1,m2,E,m3', finalizer='m1,E', finret='m1,m2,E',
    whilecond='E,m1,E,m2,E', whilebody='E,E', dowhile='E,E', ret='m1,E', retif='m1,E', retclos='m1,m2,E',
    retnest='m11,E,m12,m3', rettry='m1,E,m3', retmatch='E,m4', retunit='m1,E', retsync='m1,E', throw='E',
    throwif='m1,m2,E', byname='m1,E,m2,E', byname2='m1,E,E', byname3='m0,E', sync='m1,E', sync2='m1,E',
    asgvar='m1,E', asgfield='E,E,m7', asgarr='m1,E', asgop='m1,E,m1,E', asgopf='m6,E,m7,m6', asgopv='E,E',
    locallazy='m1,m2,E', tuplepat='E,m2', tuplepat2='m1,E', somepat='E', listpat='m1,E',
    dflt='m1,E,m2', dfltnamed='m5,m6,E', dfltnamed2='m5,E,m6', dfltctor='m3,E', dfltcurry='m4,E', ctorarg='E',
    ctorarg2='E', ctorcase='m2,E', superarg='m1,E', superarg2='m2,E,m2', superarg3='E', superauxarg='E',
    classfield='m1,E,E,m2,m9,E', privfield='E', ctorparamfield='E', objfield='m1,E,m2', objlazy='m1,E',
    traitfield='m1,E,m2', traitvar='E,E', traitlazy='E', traitobj='E', traitdef='E', early='E',
    lazyclass='m2,m1,E', localclass='E', anonclass='E', vcarg='E',
    struct='E,m1', structrecv='m1,m2,E', structstr='E', structhash='E', structeq='E,E', structtolong='E',
    structplus='E,m1', structneg='E', structlt='E,m3', structmod='E,m3', structz='E', structs='E',
    dynapply='E,m2', dynnamed='E,m2', dynupd='E', dynrecv='m1,m2,E', interp='m1,E,m2', interpf='E,m1',
    interpfd='E,E', interpraw='E', varargs='E', varargs2='m1,E', varargs3='m1,E', varargs4='E,m2',
    arrelem='E', arrelem2='E,E', arrelemany='E,m1', arridx='E', concat='E')


def expected_trace(case, ref):
    if case.ctx == 'concatlong':
        toks = [('E' if p.startswith('{ m.n += 1; $E') else 'm' + p[3:-1]) for p in case.extra[len('{ val m = new Mu; "" + '):-2].split(' + ')
                if p.startswith('mk(') or p.startswith('{ m.n += 1; $E')]
    else:
        e = EXPECT.get(case.ctx)
        if e is None:
            return None
        toks = e.split(',')
    return ''.join(ref if t == 'E' else t + ',' for t in toks)

CONTEXTS += [
    # ---- definitions whose fields the constructor phase may elide
    ('elided', 'def', ALL, 'class C { private[this] val u: $T = $E; val k = mk(1) }', '{ new C; "k" }'),
    ('elided2', 'def', ALL, 'class C { private[this] val a: $T = $E; val b = "" + a }', 'new C().b'),
    ('elided3', 'def', ALL, 'class C(p: $T) { mk(1) }', '{ new C($E); "k" }'),
    ('elided4', 'def', ALL, 'class C { private val a: $T = $E; mk(1); def g = 1 }', '{ new C().g }'),
    ('constfield', 'def', ALL, 'trait Tr { final val f = $E }; class TC extends Tr', 'new TC().f'),
    ('constobj', 'def', ALL, 'object OF { final val f = $E }', '{ mk(2); OF.f }'),
    # ---- control flow in expression position (lifted by uncurry or spilled by the back end)
    ('tryarg', 'lift', ALL, 'def g(a: Int, b: $T, c: Int): String = "" + a + b + c', 'g(mk(1), try $E finally mk(2), mk(3))'),
    ('tryargc', 'lift', ALL, 'def g(a: Int, b: $T, c: Int): String = "" + a + b + c',
     'g(mk(1), try { mk(4); $E } catch { case _: ArithmeticException => $Z }, mk(3))'),
    ('tryargret', 'lift', ALL, 'def g(a: Int, b: Any): String = "x"; def f(): $T = { g(mk(1), try { return $E } finally mk(2)); $Z }', 'f()'),
    ('tryargret2', 'lift', ALL, 'def g(a: Int, b: Any, c: Int): String = "x"; def f(): $T = { g(mk(1), try { if (mk(4) == 4) return $E; 1 } catch { case _: IllegalStateException => 2 }, mk(3)); $Z }', 'f()'),
    ('matcharg', 'lift', ALL, 'def g(a: Int, b: $T, c: Int): String = "" + a + b + c', 'g(mk(1), mk(0) match { case 0 => $E; case _ => $Z }, mk(3))'),
    ('whilearg', 'lift', ALL, 'def g(a: Int, b: $T, c: Int): String = "" + a + b + c', 'g(mk(1), { var i = 0; while (i < 1) i += 1; $E }, mk(3))'),
    ('syncarg', 'lift', ALL, 'def g(a: Int, b: $T, c: Int): String = "" + a + b + c', 'g(mk(1), this.synchronized($E), mk(3))'),
    ('tryconcat', 'lift', ALL, '', '"" + mk(1) + (try $E finally mk(2)) + mk(3)'),
    ('tryarr', 'lift', ALL, '', '{ val a = Array[Any](mk(1), try $E catch { case _: Throwable => $Z }, mk(3)); a(1) }'),
    ('trynew', 'lift', ALL, 'class C(val a: Int, val b: $T, val c: Int)', 'new C(mk(1), try $E finally mk(2), mk(3)).b'),
    ('trysuper', 'lift', ALL, 'class P(val a: Int, val b: $T); class S extends P(mk(1), try $E finally mk(2))', 'new S().b'),
    ('trysuper2', 'lift', ALL, 'class P(val b: $T); class S(x: Int) extends P(try { mk(x); $E } catch { case _: IllegalStateException => $Z })', 'new S(1).b'),
    ('tryfield', 'lift', ALL, 'trait Tr { val f: $T = try $E finally mk(1) }; class TC extends Tr', 'new TC().f'),
    ('trydflt', 'lift', ALL, 'def f(a: Int = mk(1), p: $T = try $E finally mk(2)): String = "" + a + p', 'f()'),
    ('trybyname', 'lift', ALL, 'def bn(a: => Any): Any = { mk(1); a }', 'bn(try $E finally mk(2))'),
    ('tryinterp', 'lift', ALL, '', 's"${mk(1)}${try $E finally mk(2)}${mk(3)}"'),
    ('trylambda', 'lift', ALL, '', '((x: Int) => "" + x + (try $E finally mk(2)))(mk(1))'),
    ('tryop', 'lift', NUMT, '', '(mk(1) + (try $E finally mk(2)))'),
    ('trycond', 'lift', ALL, '', '(if (mk(1) == 1 && (try { $E; true } finally mk(2))) "y" else "n")'),
    ('retbyname', 'lift', ALL, 'def bn(a: => Any): Any = { mk(1); a }; def f(): $T = { bn(return $E); $Z }', 'f()'),
    ('retlazy', 'lift', ALL, 'def f(): $T = { lazy val l: Int = return $E; mk(1); l; $Z }', 'f()'),
    ('retarg', 'lift', ALL, 'def g(a: Int, b: Any): String = "x"; def f(): $T = { g(mk(1), return $E); $Z }', 'f()'),
    ('retcond', 'lift', ALL, 'def f(): $T = { if (mk(1) == 1 && (return $E)) mk(2); $Z }', 'f()'),
    ('throwarg', 'lift', ALL, 'class X(val x: Any) extends RuntimeException; def g(a: Int, b: Any): String = "x"',
     '(try g(mk(1), throw new X($E)) catch { case e: X => e.x })'),
]
EXPECT.update(
    elided='E,m1', elided2='E', elided3='E,m1', elided4='E,m1', constfield='E', constobj='m2,E',
    tryarg='m1,E,m2,m3', tryargc='m1,m4,E,m3', tryargret='m1,E,m2', tryargret2='m1,m4,E', matcharg='m1,m0,E,m3',
    whilearg='m1,E,m3', syncarg='m1,E,m3', tryconcat='m1,E,m2,m3', tryarr='m1,E,m3', trynew='m1,E,m2,m3',
    trysuper='m1,E,m2', trysuper2='m1,E', tryfield='E,m1', trydflt='m1,E,m2', trybyname='m1,E,m2',
    tryinterp='m1,E,m2,m3', trylambda='m1,E,m2', tryop='m1,E,m2', trycond='m1,E,m2', retbyname='m1,E',
    retlazy='m1,E', retarg='m1,E', retcond='m1,E', throwarg='m1,E')
CTX = {c[0]: c for c in CONTEXTS}

CONTEXTS += [
    # ---- argument-order rewrites: named and default arguments, right-associative operators, compound assignment
    ('namedbyname', 'named', ALL, 'def f(a: => $T, b: Int): String = { mk(9); "" + a + b }', 'f(b = mk(1), a = $E)'),
    ('namedrecv', 'named', ALL, 'class O { def f(a: $T, b: Int): String = "" + a + b }', '{ mk(1); new O }.f(b = mk(2), a = $E)'),
    ('namedsuper', 'named', ALL, 'class P(val a: $T, val b: Int); class S extends P(b = mk(1), a = $E)', 'new S().a'),
    ('namedsuperdflt', 'named', ALL, 'class P(val a: $T, val b: Int = mk(5), val c: Int = mk(6)); class S extends P(c = mk(1), a = $E)', 'new S().a'),
    ('namedvarargs', 'named', ALL, 'def f(a: Int, xs: $T*): String = "" + a + xs.mkString', 'f(xs = $E, a = mk(1))'),
    ('namedcopy', 'named', ALL, 'case class C(a: $T, b: Int)', '{ val c = C($Z, mk(1)); c.copy(b = mk(2), a = $E).a }'),
    ('namedmulti', 'named', ALL, 'def f(a: Int, b: Int)(c: $T, d: Int = mk(7)): String = "" + a + b + c + d', 'f(b = mk(1), a = mk(2))(c = $E)'),
    ('namedtparam', 'named', ALL, 'def f[A, B](a: A, b: B): String = "" + a + b', 'f(b = mk(1), a = $E)'),
    ('rassoc', 'named', ALL, 'class R { def ~:(x: Any): String = { mk(2); "" + x } }', '$E ~: { mk(1); new R }'),
    ('rassocbn', 'named', ALL, 'class R { def ~:(x: => Any): String = { mk(2); "" + x } }', '$E ~: { mk(1); new R }'),
    ('rassoclist', 'named', ALL, '', '($E :: { mk(1); List[Any](mk(2)) }).head'),
    ('rassocprep', 'named', ALL, '', '($E +: { mk(1); Vector[Any]() }).head'),
    ('asgqual', 'named', ARITHT + ['T'], 'class H { var f: $T = $Z }', '{ val h = new H; { mk(1); h }.f += $E; h.f }'),
    ('asgbuf', 'named', ARITHT + ['T'], '', '{ val b = collection.mutable.ArrayBuffer[$T]($Z, $Z); val bb = { mk(2); b }; bb(mk(1)) += $E; b(1) }'),
    ('asgmap', 'named', ALL, '', '{ val m = collection.mutable.Map[Int, $T](); val mm = { mk(2); m }; mm(mk(1)) = $E; m(1) }'),
    ('asgstruct', 'named', ARITHT, 'class H { var f: $T = $Z }', '{ val h: { var f: $T } = new H; h.f += $E; h.f }'),
    ('asgcurry', 'named', ARITHT + ['T'], 'class H { private[this] var v: $T = $Z; def apply(i: Int): $T = { mk(6); v }; def update(i: Int, x: $T): Unit = { mk(7); v = x } }',
     '{ val h = new H; h(mk(1)) += $E; h(0) }'),
]
EXPECT.update(
    namedbyname='m1,m9,E', namedrecv='m1,m2,E', namedsuper='m1,E', namedsuperdflt='m1,E,m5', namedvarargs='E,m1',
    namedcopy='m1,m2,E', namedmulti='m1,m2,E,m7', namedtparam='m1,E', rassoc='E,m1,m2', rassocbn='m1,m2,E',
    rassoclist='E,m1,m2', rassocprep='E,m1', asgqual='m1,E', asgbuf='m2,m1,E', asgmap='m2,m1,E', asgdyn='m5,E',
    asgstruct='E', asgcurry='m1,m6,E,m7,m6')
CTX = {c[0]: c for c in CONTEXTS}

CONTEXTS += [
    # ---- Unit-typed and other fields the fields phase treats specially
    ('unitfield', 'def', ALL, 'class C { val u: Unit = $E; var w: Unit = $E; mk(1) }', '{ val c = new C; "" + c.u + c.w }'),
    ('unittrait', 'def', ALL, 'trait Tr { val u: Unit = $E; lazy val l: Unit = $E }; class TC extends Tr', '{ val c = new TC; mk(1); "" + c.u + c.l + c.l }'),
    ('unitobj', 'def', ALL, 'object OU { val u: Unit = $E }', '{ mk(1); OU.u }'),
    ('unitlocal', 'def', ALL, '', '{ lazy val l: Unit = $E; mk(1); l; l; "" + l }'),
    ('unitvartrait', 'def', ALL, 'trait Tr { var u: Unit = $E }; class TC extends Tr', '{ val c = new TC; c.u = $E; "" + c.u }'),
    ('moduleintrait', 'def', ALL, 'trait Tr { object O { mk(1); val f: $T = $E } }; class TC extends Tr', '{ val c = new TC; mk(2); c.O.f }'),
    ('overridelazy', 'def', ALL, 'trait Tr { lazy val f: Any = mk(5) }; class TC extends Tr { override lazy val f: Any = $E }', '{ val c = new TC; mk(1); "" + c.f + c.f }'),
    ('overrideval', 'def', ALL, 'class P { val f: Any = mk(5); val g = "" + f }; class S extends P { override val f: Any = $E }', 'new S().g'),
    ('traitprivate', 'def', ALL, 'trait Tr { private val p: $T = $E; def get = p }; class TC extends Tr', 'new TC().get'),
    ('traitabs', 'def', ALL, 'trait Tr { def a: Int; val b: $T = { mk(a); $E } }; class TC extends Tr { def a = 3 }', 'new TC().b'),
    ('traitlazyunit', 'def', ALL, 'trait Tr { lazy val l: Unit = { mk(1); $E } }; object TO extends Tr', '{ mk(2); TO.l; "" + TO.l }'),
]
EXPECT.update(unitfield='E,E,m1', unittrait='E,m1,E', unitobj='m1,E', unitlocal='m1,E', unitvartrait='E,E',
    moduleintrait='m2,m1,E', overridelazy='m1,E', overrideval='m5,E', traitprivate='E', traitabs='m3,E', traitlazyunit='m2,m1,E')
CTX = {c[0]: c for c in CONTEXTS}


def concat_long(rng):
    parts = []
    for k in range(rng.choice([25, 70, 105, 205, 230])):
        r = rng.random()
        if k in (3, 60, 150):
            parts.append('{ m.n += 1; $E }')
        elif r < 0.25:
            parts.append('mk(%d)' % k)
        elif r < 0.4:
            parts.append('m')
        elif r < 0.5:
            parts.append('%dL' % k)
        elif r < 0.6:
            parts.append('%d.5' % k)
        elif r < 0.65:
            parts.append('()')
        elif r < 0.7:
            parts.append('null')
        elif r < 0.75:
            parts.append('WI(%d)' % k)
        elif r < 0.85:
            parts.append('"s%d"' % k)
        else:
            parts.append('{ m.n += 1; m }')
    return '{ val m = new Mu; "" + ' + ' + '.join(parts) + ' }'


def lit_pattern(rng, ty):
    if ty == 'Z':
        return ['true', 'false']
    out = []
    while len(out) < 2:
        if ty in ('B', 'S', 'C'):
            lo, hi = {'B': (-128, 127), 'S': (-32768, 32767), 'C': (0, 65535)}[ty]
            t = str(rng.choice([x for x in fold.IPOOL if lo <= x <= hi]))
        else:
            p = fold.lit(rng, ty)
            if not p.data[1] or 'null' in p.data[0]:
                continue
            t = p.data[0]
        if t not in out:
            out.append(t)
    return out


def trace_leaves(tree, rng, p=0.4):
    """wraps a random share of the leaves in a traced identity, numbered in pre-order"""
    k = [0]

    def go(n):
        if n.kind == 'lit':
            if n.ty in ('U', 'N') or rng.random() >= p:
                return n
            k[0] += 1
            return N('lit', n.ty, data=('t%s(%d, %s)' % (n.ty, k[0], n.data[0]), False))
        return N(n.kind, n.ty, [go(c) for c in n.kids], n.data)
    return go(tree)


def expr_text(tree):
    return fold.render(tree, fold.Ctx('lit', None, False, 'x'))


class Case:
    def __init__(self, cid, ctx, tree, pats, extra=None):
        self.cid, self.ctx, self.tree, self.pats, self.extra = cid, ctx, tree, pats, extra


def make_case(rng, cid, families, depth, ctxname=None):
    names = [c[0] for c in CONTEXTS if c[1] in families] if ctxname is None else [ctxname]
    name = rng.choice(names)
    _, fam, types, _, _ = CTX[name]
    ty = rng.choice(types or ['I', 'I', 'J', 'F', 'D', 'Z', 'Z', 'T', 'T', 'B', 'S', 'C'])
    tree = trace_leaves(fold.gen(rng, ty, rng.randint(1, depth)), rng)
    pats = lit_pattern(rng, ty) if ty != 'F' and ty != 'D' else None
    extra = concat_long(rng) if name == 'concatlong' else None
    return Case(cid, name, tree, pats, extra)


def fill(tmpl, case, hole):
    ty = case.tree.ty
    T = TNAME[ty]
    s = tmpl.replace('CONCATLONG', case.extra or 'CONCATLONG')
    s = s.replace('$T2', T).replace('$T', T).replace('$Z', ZERO[ty])
    s = s.replace('$R', TNAME[fold.promote(ty, 'I')] if ty in NUMT else T)
    if case.pats:
        s = s.replace('$P1', case.pats[0]).replace('$P2', case.pats[1])
    return s.replace('$E', hole)


def holes(case, s, owner):
    e = expr_text(case.tree)
    T = TNAME[case.tree.ty]
    return {
        'i': '(%s: %s)' % (e, T),
        'v': '{ val v_ : %s = %s; v_ }' % (T, e),
        'm': '%s.e()' % owner,
        'f': '(() => (%s: %s)).apply()' % (e, T),
    }[s]


TRACERS = ''.join('  @noinline def t%s(k: Int, x: %s): %s = { log(k); x }\n' % (t, TNAME[t], TNAME[t])
                  for t in NUM + ['Z', 'T'])
RT2 = '''
object Tr {
  val buf = new java.lang.StringBuilder
  def reset(): Unit = buf.setLength(0)
  def dump(): String = buf.toString
  def log(k: Int): Unit = buf.append(k).append(',')
  @noinline def mk(k: Int): Int = { buf.append('m').append(k).append(','); k }
''' + TRACERS + '}\n'


def emit(cases, spells, skip=()):
    out = [fold.PRELUDE, RT2, 'import scala.language.{reflectiveCalls, dynamics, postfixOps}', 'import Rt._, Tr._']
    linemap = {}
    calls = []
    for case in cases:
        _, _, _, decls, expr = CTX[case.ctx]
        e = expr_text(case.tree)
        T = TNAME[case.tree.ty]
        out.append('object X%d { @noinline def e(): %s = (%s: %s) }' % (case.cid, T, e, T))
        calls.append('println("%d r " + { reset(); try show(X%d.e()) catch { case e: Throwable => ex(e) } } + " " + dump())' % (case.cid, case.cid))
        for s in spells:
            if (case.cid, s) in skip:
                continue
            h = holes(case, s, 'X%d' % case.cid)
            o = 'D%d%s' % (case.cid, s)
            start = len('\n'.join(out).split('\n')) + 1
            out.append('object %s {' % o)
            if decls:
                out.append('  ' + fill(decls, case, h))
            out.append('  def run(): String = { reset(); val r = try show(%s) catch { case e: Throwable => ex(e) }; r + " " + dump() }'
                       % fill(expr, case, h))
            out.append('}')
            for ln in range(start, start + 4):
                linemap[ln] = (case.cid, s)
            calls.append('println("%d %s " + %s.run())' % (case.cid, s, o))
    out.append('object Main {')
    nm = 0
    for i in range(0, len(calls), 150):
        out.append('  def m%d(): Unit = {' % nm)
        out += ['    ' + c for c in calls[i:i + 150]]
        out.append('  }')
        nm += 1
    out.append('  def main(args: Array[String]): Unit = { %s }' % '; '.join('m%d()' % j for j in range(nm)))
    out.append('}')
    src = '\n'.join(out) + '\n'
    return src, linemap


OPT = {'noopt': [], 'opt': ['-opt:inline:**', '-opt:local']}


def compile_run(src, workdir, opt, linemap, timeout=230):
    os.makedirs(workdir, exist_ok=True)
    f = os.path.join(workdir, 'Main.scala')
    out = os.path.join(workdir, 'out-' + opt)
    if os.path.isdir(out):
        shutil.rmtree(out)
    os.makedirs(out)
    open(f, 'w').write(src)
    b = BUILDS[BUILD]
    env = fold.ENV
    if JACOCO:
        env = dict(env, JAVA_OPTS='-Xmx2g -javaagent:%s=destfile=%s,includes=scala.*,inclbootstrapclasses=true,append=true' % (fold.AGENT, JACOCO))
    try:
        p = subprocess.run([b + '/bin/scalac', '-nowarn', '-Xmaxerrs', '100000', '-d', out] + OPT[opt] + [f],
                           capture_output=True, text=True, env=env, timeout=timeout)
    except subprocess.TimeoutExpired:
        return {}, {}, 'compile timeout'
    txt = p.stdout + p.stderr
    if p.returncode != 0:
        rej = {}
        for m in re.finditer(r'Main\.scala:(\d+): error: (.*)', txt):
            ln = int(m.group(1))
            if ln in linemap:
                rej.setdefault(linemap[ln], m.group(2))
        if not rej:
            return {}, {}, 'CRASH ' + txt[-4000:]
        return {}, rej, txt[-3000:]
    try:
        r = subprocess.run(['java', '-Xmx1g', '-Xss4m', '-cp', b + '/lib/scala-library.jar:' + out, 'Main'],
                           capture_output=True, text=True, env=fold.ENV, timeout=120)
    except subprocess.TimeoutExpired:
        return {}, {}, 'run timeout'
    res = {}
    for line in r.stdout.split('\n'):
        m = re.match(r'(\d+) ([a-z]) (.*)', line)
        if m:
            res[(int(m.group(1)), m.group(2))] = m.group(3)
    if r.returncode != 0:
        return res, {}, 'RUNFAIL ' + r.stderr[-2000:]
    return res, {}, None


def evaluate(cases, spells, workdir, opts=('noopt', 'opt')):
    rejects, table, errors = {}, {}, []
    for opt in opts:
        for attempt in range(6):
            src, linemap = emit(cases, spells, skip=rejects)
            res, rej, err = compile_run(src, workdir, opt, linemap)
            if rej:
                rejects.update(rej)
                continue
            if err:
                errors.append((opt, err))
            for (cid, s), val in res.items():
                table.setdefault(cid, {})[(s, opt)] = val
            break
    for (cid, s) in rejects:
        for o in opts:
            table.get(cid, {}).pop((s, o), None)
    return table, rejects, errors


def disagreements(row):
    by = {}
    for k, v in row.items():
        if k[0] != 'r':
            by.setdefault(v, []).append(k)
    return by if len(by) > 1 else None


def trace_failures(case, row):
    """spellings whose trace differs from the context's specified trace, as {trace: [keys]} with the expected one"""
    ref = row.get(('r', 'noopt'))
    if ref is None or ref.startswith('!') or ' ' not in ref:
        return None
    exp = expected_trace(case, ref.rsplit(' ', 1)[1])
    if exp is None:
        return None
    by = {}
    for k, v in row.items():
        if k[0] == 'r' or v.startswith('!'):
            continue
        t = v.rsplit(' ', 1)[1] if ' ' in v else ''
        if t != exp:
            by.setdefault(v, []).append(k)
    if not by:
        return None
    by['expected trace ' + exp] = []
    return by


def failure(case, row):
    by = disagreements(row)
    if by:
        return signature(case.ctx, by), by
    by = trace_failures(case, row)
    if by:
        return 'TRACE %s' % case.ctx, by
    return None


def signature(ctx, by):
    groups = sorted(''.join(sorted(set(s + ('' if o == 'noopt' else "'") for s, o in ks))) for ks in by.values())
    vals = list(by)
    what = 'value' if len(set(v.split(' ')[0] for v in vals)) > 1 else 'trace'
    return '%s %s %s' % (ctx, '|'.join(groups), what)


def candidates(case, rng):
    out = []
    for t in fold.candidates(case.tree, rng):
        out.append(Case(0, case.ctx, t, case.pats, case.extra))
    return out


def shrink(case, spells, workdir, rng, rounds=10):
    cur = case
    for _ in range(rounds):
        cands = candidates(cur, rng)[:60]
        if not cands:
            break
        for i, c in enumerate(cands):
            c.cid = i
        table, _, _ = evaluate(cands, spells, workdir)
        failing = [cands[i] for i in sorted(table) if failure(cands[i], table[i])]
        if not failing:
            break
        cur = min(failing, key=lambda c: c.tree.size())
    return cur


def main():
    global JACOCO, BUILD
    ap = argparse.ArgumentParser()
    ap.add_argument('--seed', type=int, default=1)
    ap.add_argument('--n', type=int, default=150)
    ap.add_argument('--batches', type=int, default=100)
    ap.add_argument('--depth', type=int, default=4)
    ap.add_argument('--families', default='ctl')
    ap.add_argument('--ctx', help='only this context')
    ap.add_argument('--work', required=True)
    ap.add_argument('--jacoco')
    ap.add_argument('--build', default='int')
    ap.add_argument('--budget', type=int, default=600)
    ap.add_argument('--max-shrink', type=int, default=4)
    ap.add_argument('--out', help='json summary')
    a = ap.parse_args()
    JACOCO, BUILD = a.jacoco, a.build
    fams = a.families.split(',')
    t0 = time.time()
    sigs, rejsigs, errs = {}, {}, []
    total = 0
    allrej = 0
    for b in range(a.batches):
        if time.time() - t0 > a.budget:
            break
        rng = random.Random(a.seed * 1000 + b)
        cases = [make_case(rng, i, fams, a.depth, a.ctx) for i in range(a.n)]
        table, rejects, errors = evaluate(cases, SPELL, os.path.join(a.work, 'b%d' % b))
        total += len(cases)
        byid = {c.cid: c for c in cases}
        for opt, e in errors:
            errs.append((a.seed, b, opt, e[:600]))
            print('ERROR batch', b, opt, e[:300], file=sys.stderr)
        rej_by_case = {}
        for (cid, s), msg in rejects.items():
            rej_by_case.setdefault(cid, []).append((s, msg))
        for cid, lst in rej_by_case.items():
            if len(lst) == len(SPELL):
                allrej += 1
                key = 'ALLREJ %s %s' % (byid[cid].ctx, re.sub(r'\d+', 'N', lst[0][1])[:70])
            else:
                key = 'REJECT %s %s %s' % (byid[cid].ctx, ''.join(sorted(s for s, _ in lst)), re.sub(r'\d+', 'N', lst[0][1])[:70])
            rejsigs.setdefault(key, []).append((b, byid[cid]))
        nf = 0
        for cid in sorted(table):
            f = failure(byid[cid], table[cid])
            if f:
                nf += 1
                sigs.setdefault(f[0], []).append((b, byid[cid], f[1]))
        print('batch %d: %d cases, %d rejected spellings, %d failing, %.0fs' % (
            b, len(cases), len(rejects), nf, time.time() - t0), file=sys.stderr)
    secs = time.time() - t0
    print('cases %d seconds %.0f signatures %d reject-signatures %d all-rejected %d errors %d' % (
        total, secs, len(sigs), len(rejsigs), allrej, len(errs)))
    for k, items in sorted(rejsigs.items(), key=lambda kv: -len(kv[1])):
        b, c = items[0]
        print('\n== %s (%d)\n  %s' % (k, len(items), fill(CTX[c.ctx][4], c, holes(c, 'i', 'X'))[:400]))
    rng = random.Random(a.seed)
    ns = 0
    for sig, items in sorted(sigs.items(), key=lambda kv: -len(kv[1])):
        b, case, by = items[0]
        print('\n== %s  (%d cases) first batch %d id %d' % (sig, len(items), b, case.cid))
        if ns < a.max_shrink:
            ns += 1
            case = shrink(case, SPELL, os.path.join(a.work, 'shrink'), rng)
            case.cid = 0
            t, _, _ = evaluate([case], SPELL, os.path.join(a.work, 'shrink'))
            f = failure(case, t.get(0, {}))
            by = f[1] if f else by
        print('  decls: ' + fill(CTX[case.ctx][3], case, holes(case, 'i', 'X0')))
        print('  expr:  ' + fill(CTX[case.ctx][4], case, holes(case, 'i', 'X0'))[:600])
        for val, ks in by.items():
            print('  %-50s <- %s' % (val[:200], ' '.join(sorted(s + ('' if o == 'noopt' else "'") for s, o in ks))))
    for e in errs[:10]:
        print('\n== ERROR seed %d batch %d %s\n%s' % e)
    if a.out:
        json.dump({'cases': total, 'seconds': round(secs), 'signatures': sorted(sigs), 'rejects': {k: len(v) for k, v in rejsigs.items()},
                   'errors': len(errs)}, open(a.out, 'w'), indent=1)


if __name__ == '__main__':
    main()
