#!/usr/bin/env python3
"""Metamorphic fuzzer for primitive expression semantics in scalac 2.13.x.

Generates well-typed expression trees over Byte/Short/Char/Int/Long/Float/Double/Boolean/String,
renders each tree in several forms that must agree at run time, compiles one unit per batch
with and without the optimizer, runs both and diffs the `id variant value` lines.
"""
import argparse, os, random, re, subprocess, sys, shutil, json

HOME = os.path.expanduser('~')
JAVA_HOME = HOME + '/.sdkman/candidates/java/21.0.12+1.1-tem'
AGENT = HOME + '/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/jacoco/org.jacoco.agent/0.8.13/org.jacoco.agent-0.8.13-runtime.jar'
BUILDS = {
    'int': '/Users/jzaugg-virtus/code/.worktrees/scala/w3-int/build/pack',
    'base': '/Users/jzaugg-virtus/code/.worktrees/scala/w3-base/build/pack',
}
JACOCO = None
ENV = dict(os.environ, JAVA_HOME=JAVA_HOME, PATH=JAVA_HOME + '/bin:' + os.environ['PATH'], JAVA_OPTS='-Xmx2g')

TNAME = {'B': 'Byte', 'S': 'Short', 'C': 'Char', 'I': 'Int', 'J': 'Long', 'F': 'Float', 'D': 'Double',
         'Z': 'Boolean', 'T': 'String', 'U': 'Unit', 'N': 'Null'}
JBOX = {'B': 'java.lang.Byte', 'S': 'java.lang.Short', 'C': 'java.lang.Character', 'I': 'java.lang.Integer',
        'J': 'java.lang.Long', 'F': 'java.lang.Float', 'D': 'java.lang.Double', 'Z': 'java.lang.Boolean',
        'T': 'String', 'U': 'Any', 'N': 'AnyRef'}
NUM = ['B', 'S', 'C', 'I', 'J', 'F', 'D']
SUBI = ['B', 'S', 'C', 'I']
INTEG = ['B', 'S', 'C', 'I', 'J']
RANK = {'B': 1, 'S': 2, 'C': 2, 'I': 3, 'J': 4, 'F': 5, 'D': 6}
# weak conformance: which types weakly conform to key
WEAK = {'B': ['B'], 'S': ['B', 'S'], 'C': ['C'], 'I': ['B', 'S', 'C', 'I'], 'J': ['B', 'S', 'C', 'I', 'J'],
        'F': ['B', 'S', 'C', 'I', 'J', 'F'], 'D': NUM, 'Z': ['Z'], 'T': ['T']}


def promote(a, b):
    return {3: 'I', 4: 'J', 5: 'F', 6: 'D'}[max(RANK[a], RANK[b], 3)]


class N:
    __slots__ = ('kind', 'ty', 'kids', 'data')

    def __init__(self, kind, ty, kids=(), data=None):
        self.kind, self.ty, self.kids, self.data = kind, ty, list(kids), data

    def size(self):
        return 1 + sum(k.size() for k in self.kids)


IPOOL = [0, 1, -1, 2, 3, 7, 31, 32, 33, 63, 64, 65, 127, 128, -128, -129, 255, 256, 32767, 32768, -32768, 65535, 65536,
         2147483647, -2147483648, -2147483647, 0x55555555, 16777217, 1 << 30, 97]
JPOOL = IPOOL + [1 << 31, 1 << 32, (1 << 32) - 1, -(1 << 32), 1 << 53, (1 << 53) + 1, (1 << 63) - 1, -(1 << 63),
                 -(1 << 63) + 1, 1 << 62, 0x5555555555555555]
FPOOL2 = ['1e3f', '.5f', '5f', '1_0.2_5f', '1E-3F', '0x10.toFloat', '16777217f', '0.1f', '1.17549435E-38f', '3.4028235e38f']
DPOOL2 = ['1e3', '.5', '5d', '5D', '1_0.2_5', '1E-3', '1e3d', '4.9e-324', '0.1f.toDouble', '1.7976931348623157e308', '2.2250738585072014E-308', '9007199254740993.0']
FPOOL = ['0.0f', '-0.0f', '1.0f', '-1.0f', '0.5f', '1.5f', '-2.5f', '3.0f', '1.0E10f', '-1.0E10f', '1.0E-10f',
         '2.14748365E9f', '-2.14748365E9f', '9.223372E18f', '1.6777216E7f', '65535.0f', '127.5f', '-128.5f',
         'Float.NaN', 'Float.MaxValue', 'Float.MinValue', 'Float.MinPositiveValue', 'Float.PositiveInfinity',
         'Float.NegativeInfinity', '1.4E-45f', '0.1f', '3.4028235E38f']
DPOOL = ['0.0', '-0.0', '1.0', '-1.0', '0.5', '1.5', '-2.5', '3.0', '1.0E10', '-1.0E10', '1.0E-10', '2.147483647E9',
         '2.147483648E9', '-2.147483649E9', '9.223372036854775807E18', '-9.223372036854775808E18', '1.6777217E7',
         '9.007199254740993E15', '65535.5', '127.5', '-128.5', 'Double.NaN', 'Double.MaxValue', 'Double.MinValue',
         'Double.MinPositiveValue', 'Double.PositiveInfinity', 'Double.NegativeInfinity', '4.9E-324', '0.1',
         '1.0E100', '3.4028235E38', '1.0E-320']
TPOOL = ['""', '"a"', '"abc"', '"null"', '"1"', '"1.0"', '"true"', '"\\u0000"', '" "', '(null: String)', '"NaN"', '"-0.0"']


def lit(rng, ty):
    """returns N('lit') whose data is (text, plain) where plain means text is a syntactic literal"""
    if EVOLVED and ty in NUM + ['Z'] and rng.random() < 0.03:
        return N('lit', ty, data=('null.asInstanceOf[%s]' % TNAME[ty], False))
    if ty == 'I':
        v = rng.choice(IPOOL) if rng.random() < 0.8 else rng.randint(-2 ** 31, 2 ** 31 - 1)
        if v == -2 ** 31 and rng.random() < 0.5:
            return N('lit', ty, data=('Int.MinValue', False))
        if EVOLVED and rng.random() < 0.3:
            u = v & 0xffffffff
            t = rng.choice([format(u, '#x'), '0X' + format(u, 'X'), format(u, '#b'), '0x' + format(u, '_x'),
                            format(v, '_d')])
            return N('lit', ty, data=(t, True))
        return N('lit', ty, data=(str(v), True))
    if ty == 'J':
        v = rng.choice(JPOOL) if rng.random() < 0.8 else rng.randint(-2 ** 63, 2 ** 63 - 1)
        if v == -2 ** 63 and rng.random() < 0.5:
            return N('lit', ty, data=('Long.MinValue', False))
        return N('lit', ty, data=(str(v) + 'L', True))
    if ty in ('B', 'S', 'C'):
        lo, hi = {'B': (-128, 127), 'S': (-32768, 32767), 'C': (0, 65535)}[ty]
        pool = [x for x in IPOOL if lo <= x <= hi] + [lo, hi]
        v = rng.choice(pool) if rng.random() < 0.8 else rng.randint(lo, hi)
        r = rng.random()
        if ty == 'C' and 48 <= v < 123 and chr(v).isalnum() and r < 0.5:
            return N('lit', ty, data=("'%s'" % chr(v), True))
        if r < 0.75:
            return N('lit', ty, data=('(%d: %s)' % (v, TNAME[ty]), False))
        # folded narrowing conversion from an Int outside or inside the range
        w = v + rng.choice([0, 0, 65536, -65536, 1 << 24])
        if not (-2 ** 31 <= w < 2 ** 31):
            w = v
        return N('lit', ty, data=('(%d).to%s' % (w, TNAME[ty]), False))
    if ty == 'F':
        t = rng.choice(FPOOL + (FPOOL2 if EVOLVED else []))
        return N('lit', ty, data=(t, (t[0].isdigit() or t[0] == '-') and '.to' not in t))
    if ty == 'D':
        t = rng.choice(DPOOL + (DPOOL2 if EVOLVED else []))
        return N('lit', ty, data=(t, (t[0].isdigit() or t[0] == '-') and '.to' not in t))
    if ty == 'Z':
        return N('lit', ty, data=(rng.choice(['true', 'false']), True))
    if ty == 'T':
        t = rng.choice(TPOOL)
        return N('lit', ty, data=(t, t.startswith('"')))
    if ty == 'U':
        return N('lit', ty, data=('()', False))
    if ty == 'N':
        return N('lit', ty, data=('null', False))
    raise Exception(ty)


EVOLVED = False
ANYT = NUM + ['Z', 'T']
ARITH = ['+', '-', '*', '/', '%']
BITS = ['&', '|', '^']
SHIFTS = ['<<', '>>', '>>>']
CMP = ['<', '<=', '>', '>=']


def pick_pair(rng, ty):
    """operand types whose binary numeric promotion is ty"""
    if ty == 'I':
        return rng.choice(SUBI), rng.choice(SUBI)
    lower = [t for t in NUM if RANK[t] <= RANK[ty]]
    a, b = ty, rng.choice(lower)
    return (a, b) if rng.random() < 0.5 else (b, a)


def gen(rng, ty, d):
    if d <= 0 or rng.random() < 0.12 or ty in ('U', 'N'):
        return lit(rng, ty)
    g = lambda t: gen(rng, t, d - 1)
    anyt = ANYT + (['U', 'N', 'U', 'N'] if EVOLVED else [])
    if EVOLVED:
        r = rng.random()
        if r < 0.04:
            return N('ifthrow', ty, [g('Z'), g(ty)])
        if r < 0.08:
            return N('block', ty, [g(rng.choice(anyt)), g(ty)])
        if r < 0.11:
            return N('try', ty, [g(ty), g(ty)])
        if r < 0.14:
            return N('while', ty, [g(ty), g(ty)])
        if ty == 'I' and r < 0.20:
            ts = rng.sample(['Int', 'Long', 'Char', 'Byte', 'Short', 'Float', 'Double', 'Boolean', 'String', 'Unit',
                             'AnyRef', 'java.lang.Number', 'java.lang.Integer'], 3)
            return N('tmatch', ty, [g(rng.choice(anyt))], data=ts)
        if ty == 'I' and r < 0.25:
            ps = rng.sample(['1', '1L', "'a'", '97', '1.0', '1.0f', '0', '-0.0', '0.0', '"a"', 'true', '()', 'null',
                             '65536', '2147483647', '2.14748365E9f', '"1"', "'1'", '49'], 3)
            return N('amatch', ty, [g(rng.choice(anyt))], data=ps)
        if ty == 'Z' and r < 0.20:
            t = rng.choice(['Int', 'Long', 'Char', 'Byte', 'Short', 'Float', 'Double', 'Boolean', 'String', 'Unit',
                            'AnyRef', 'java.lang.Number', 'java.lang.Integer', 'Any'])
            return N('isinst', ty, [g(rng.choice(anyt))], data=t)
    r = rng.random()
    # productions common to all types
    if r < 0.07:
        a, b = ty, rng.choice(WEAK[ty])
        if rng.random() < 0.5:
            a, b = b, a
        return N('if', ty, [g('Z'), g(a), g(b)])
    if r < 0.13:
        st = rng.choice(['I', 'I', 'C', 'T', 'J', 'B', 'S', 'Z'])
        scrut = g(st)
        if has_throw_tail(scrut):
            scrut = lit(rng, st)  # avoids the known back-end crash on a match whose scrutinee folds to Nothing
        pats, seen = [], set()
        while len(pats) < 2:
            if st in ('B', 'S', 'C'):
                lo, hi = {'B': (-128, 127), 'S': (-32768, 32767), 'C': (0, 65535)}[st]
                t = str(rng.choice([x for x in IPOOL if lo <= x <= hi]))
            else:
                p = lit(rng, st)
                if not p.data[1]:
                    continue
                t = p.data[0]
            if t in seen:
                continue
            seen.add(t)
            pats.append(t)
        if st == 'Z':
            pats = ['true', 'false']
        return N('match', ty, [scrut, g(ty), g(ty), g(ty)], data=pats)
    if ty in ('I', 'J', 'F', 'D'):
        r = rng.random()
        if r < 0.40:
            a, b = pick_pair(rng, ty)
            ops = ARITH + (BITS if ty in ('I', 'J') else [])
            return N('bin', ty, [g(a), g(b)], data=rng.choice(ops))
        if r < 0.55 and ty in ('I', 'J'):
            a = rng.choice(SUBI) if ty == 'I' else 'J'
            return N('bin', ty, [g(a), g(rng.choice(INTEG))], data=rng.choice(SHIFTS))
        if r < 0.65:
            a = rng.choice(SUBI) if ty == 'I' else ty
            ops = ['-', '+'] + (['~'] if ty in ('I', 'J') else [])
            return N('un', ty, [g(a)], data=rng.choice(ops))
        if r < 0.88:
            return N('conv', ty, [g(rng.choice(NUM))])
        if ty == 'I':
            if r < 0.96:
                return N('hash', ty, [g(rng.choice(anyt))], data=rng.choice(['##', '##', 'hashCode']))
            return N('len', ty, [g('T')])
        return N('conv', ty, [g(rng.choice(NUM))])
    if ty in ('B', 'S', 'C'):
        return N('conv', ty, [g(rng.choice(NUM))])
    if ty == 'Z':
        r = rng.random()
        if r < 0.30:
            return N('bin', ty, [g(rng.choice(NUM)), g(rng.choice(NUM))], data=rng.choice(CMP))
        if r < 0.60:
            k = rng.random()
            if k < 0.6:
                a, b = rng.choice(NUM), rng.choice(NUM)
            elif k < 0.8:
                a = b = rng.choice(['Z', 'T'])
            else:
                a, b = rng.choice(anyt), rng.choice(anyt)
            return N('eq', ty, [g(a), g(b)], data=rng.choice(['==', '!=']))
        if r < 0.72:
            return N('equals', ty, [g(rng.choice(anyt)), g(rng.choice(anyt))])
        if r < 0.90:
            return N('bin', ty, [g('Z'), g('Z')], data=rng.choice(['&&', '||', '&', '|', '^']))
        return N('un', ty, [g('Z')], data='!')
    if ty == 'T':
        r = rng.random()
        if r < 0.55:
            return N('concat', ty, [g('T'), g(rng.choice(anyt))])
        if r < 0.80:
            return N('concat', ty, [g(rng.choice(NUM + ['Z'])), g('T')])
        return N('tostr', ty, [g(rng.choice(NUM + ['Z'] + (['U', 'N'] if EVOLVED else [])))])
    raise Exception(ty)


ZERO = {'B': '0', 'S': '0', 'C': '0', 'I': '0', 'J': '0L', 'F': '0f', 'D': '0.0', 'Z': 'false', 'U': '()'}

# ---------------------------------------------------------------- rendering

TC_NUM = {'+': 'plus', '-': 'minus', '*': 'times'}
TC_ORD = {'<': 'lt', '<=': 'lteq', '>': 'gt', '>=': 'gteq'}


class Ctx:
    def __init__(self, leaf, box, asinst, prefix):
        self.leaf, self.box, self.asinst, self.prefix = leaf, box, asinst, prefix
        self.decls = []   # member declarations for var / fval modes
        self.locals = []  # local vals
        self.k = 0
        self.ops = {'tc': 'tc', 'asg': 'asg'}.get(leaf)
        if self.ops:
            self.leaf = 'var' if leaf == 'tc' else 'lit'


def rleaf(n, c):
    text, plain = n.data
    T = TNAME[n.ty]
    if c.leaf == 'lit':
        return '(%s)' % text
    if c.leaf == 'id':
        return 'id[%s](%s)' % (T, text)
    name = '%s_%d' % (c.prefix, c.k)
    c.k += 1
    if c.leaf == 'var':
        c.decls.append('var %s: %s = %s' % (name, T, text))
        return name
    if c.leaf == 'lval':
        c.locals.append('val %s: %s = %s' % (name, T, text))
        return name
    if c.leaf == 'dflt':
        c.decls.append('def %s(p: %s = %s): %s = p' % (name, T, text, T))
        return name + '()'
    if c.leaf == 'arr':
        if n.ty == 'N':
            return 'Array[AnyRef](null).apply(0)'
        return ('Array[%s](%s).apply(0)' if c.k % 2 else '{ val a_ = new Array[%s](2); a_(1) = %s; a_(1) }') % (T, text)
    if c.leaf == 'opt' and n.ty == 'N':
        return 'Some[Null](null).value'
    if c.leaf == 'opt':
        return ('Option[%s](%s).get' if c.k % 3 == 0 else 'Some[%s](%s).value' if c.k % 3 == 1 else 'List[%s](%s).head') % (T, text)
    if c.leaf == 'meth':
        c.decls.append('def %s(p: %s): %s = p' % (name, T, T))
        return '%s(%s)' % (name, text)
    if c.leaf == 'tup':
        return '(%s, %s)._%d' % ((text, '0', 1) if c.k % 2 else ('"s"', text, 2))
    if c.leaf == 'ref':
        c.locals.append('var %s: %s = %s' % (name, T, ZERO.get(n.ty, 'null')))
        c.locals.append('(() => %s = %s).apply()' % (name, text))
        return name
    if c.leaf == 'vc':
        if n.ty in ('U', 'N'):
            return '(%s)' % text
        return ('new V%s(%s).x' if c.k % 2 else 'W%s(%s).x') % (n.ty, text)
    if c.leaf in ('fval', 'sep'):
        if c.leaf == 'sep':
            c.decls.append('final val %s = %s' % (name, text))
            return 'KSep.' + name
        if plain and n.ty != 'B' and c.k % 2 == 0:
            c.decls.append('val %s: %s = %s' % (name, text, text))  # literal type
        else:
            c.decls.append('final val %s = %s' % (name, text))
        return 'K.' + name
    raise Exception(c.leaf)


def tail(n):
    while n.kind in ('block', 'ifthrow', 'try', 'while'):
        n = n.kids[-1]
    return n


def has_throw_tail(n):
    while n.kind in ('block', 'ifthrow', 'try', 'while'):
        if n.kind == 'ifthrow':
            return True
        n = n.kids[-1]
    return False


def bx(s, ty, c, n=None):
    if c.box == 'any':
        return '(%s: Any)' % s
    # an expected type changes how if/match branches are typed, so those are boxed as Any
    if c.box == 'java' and n is not None and tail(n).kind in ('if', 'match'):
        return '(%s: Any)' % s
    if c.box == 'java':
        return '(%s: %s)' % (s, JBOX[ty])
    return s


def render(n, c):
    k = n.kind
    R = lambda x: render(x, c)
    if k == 'lit':
        return rleaf(n, c)
    if k == 'bin':
        a, b, op = n.kids[0], n.kids[1], n.data
        if c.ops == 'tc':
            if a.ty == b.ty == n.ty and n.ty in 'IJFD':
                if op in TC_NUM:
                    return 'implicitly[Numeric[%s]].%s(%s, %s)' % (TNAME[n.ty], TC_NUM[op], R(a), R(b))
                if op in '/%' and n.ty in 'IJ':
                    return 'implicitly[Integral[%s]].%s(%s, %s)' % (TNAME[n.ty], {'/': 'quot', '%': 'rem'}[op], R(a), R(b))
                if op == '/':
                    return 'implicitly[Fractional[%s]].div(%s, %s)' % (TNAME[n.ty], R(a), R(b))
            if n.ty == 'Z' and a.ty == b.ty and a.ty in 'BSCIJ' and op in TC_ORD:
                return 'implicitly[Ordering[%s]].%s(%s, %s)' % (TNAME[a.ty], TC_ORD[op], R(a), R(b))
        if c.ops == 'asg' and a.ty == n.ty and n.ty in 'IJFDZ' and op not in ('&&', '||', '<', '<=', '>', '>='):
            return '{ var t_ : %s = %s; t_ %s= %s; t_ }' % (TNAME[n.ty], R(a), op, R(b))
        return '(%s %s %s)' % (R(a), op, R(b))
    if k == 'un':
        if c.ops == 'tc' and n.data == '-' and n.kids[0].ty == n.ty:
            return 'implicitly[Numeric[%s]].negate(%s)' % (TNAME[n.ty], R(n.kids[0]))
        return '(%s%s)' % (n.data, R(n.kids[0]))
    if k == 'conv':
        if c.asinst and n.kids[0].ty != n.ty:
            return '%s.asInstanceOf[%s]' % (R(n.kids[0]), TNAME[n.ty])
        return '%s.to%s' % (R(n.kids[0]), TNAME[n.ty])
    if k == 'hash':
        return '(%s.%s)' % (bx(R(n.kids[0]), n.kids[0].ty, c, n.kids[0]), n.data)
    if k == 'len':
        return '%s.length' % R(n.kids[0])
    if k == 'tostr':
        return '%s.toString' % bx(R(n.kids[0]), n.kids[0].ty, c, n.kids[0])
    if k == 'eq':
        a, b = n.kids
        return '(%s %s %s)' % (bx(R(a), a.ty, c, a), n.data, bx(R(b), b.ty, c, b))
    if k == 'equals':
        a, b = n.kids
        return '%s.equals(%s)' % (bx(R(a), a.ty, c, a), bx(R(b), b.ty, c, b))
    if k == 'concat':
        a, b = n.kids
        if a.ty == 'T':
            return '(%s + %s)' % (R(a), bx(R(b), b.ty, c, b))
        if c.box == 'any':
            return '(%s + %s)' % (bx(R(a), a.ty, c), R(b))
        return '(%s + %s)' % (R(a), R(b))
    if k == 'ifthrow':
        return '(if (%s) %s else throw new IllegalStateException("t"))' % (R(n.kids[0]), R(n.kids[1]))
    if k == 'try':
        return '(try %s catch { case _: ArithmeticException => %s })' % (R(n.kids[0]), R(n.kids[1]))
    if k == 'while':
        return '{ var i_ = 0; var a_ : %s = %s; while (i_ < 2) { i_ += 1; a_ = %s }; a_ }' % (
            TNAME[n.ty], R(n.kids[0]), R(n.kids[1]))
    if k == 'block':
        return '{ val u_ = %s; %s }' % (R(n.kids[0]), R(n.kids[1]))
    if k == 'tmatch':
        return '((%s: Any) match { case _: %s => 1; case _: %s => 2; case _: %s => 3; case _ => 0 })' % (
            (R(n.kids[0]),) + tuple(n.data))
    if k == 'amatch':
        return '((%s: Any) match { case %s => 1; case %s => 2; case %s => 3; case _ => 0 })' % (
            (R(n.kids[0]),) + tuple(n.data))
    if k == 'isinst':
        return '(%s: Any).isInstanceOf[%s]' % (R(n.kids[0]), n.data)
    if k == 'if':
        return '(if (%s) %s else %s)' % tuple(R(x) for x in n.kids)
    if k == 'match':
        s, e1, e2, e3 = n.kids
        return '(%s match { case %s => %s; case %s => %s; case _ => %s })' % (
            R(s), n.data[0], R(e1), n.data[1], R(e2), R(e3))
    raise Exception(k)


# variant -> (leaf mode, box mode, asInstanceOf conversions)
VARIANTS = {
    'A': ('lit', None, False),    # typer folds
    'B': ('var', None, False),    # back end evaluates
    'C': ('id', None, False),     # opaque generic identity
    'L': ('lval', None, False),   # local vals: optimizer may propagate
    'F': ('fval', None, False),   # final val constants and literal-typed vals in another object
    'V': ('var', None, True),     # conversions as asInstanceOf
    'W': ('lit', None, True),
    'X': ('var', 'any', False),   # ==, ##, equals, hashCode, toString, string + on Any
    'Y': ('lit', 'java', False),  # same on java.lang boxes
    'Z': ('lit', 'any', False),
    # evolved
    'G': ('sep', None, False),    # final vals in a separately compiled unit
    'P': ('dflt', None, False),   # each leaf a default argument
    'T': ('tup', None, False),    # each leaf through a tuple
    'R': ('ref', None, False),    # each leaf through a captured local var
    'M': ('vc', None, False),     # each leaf through a value class
    'E': ('arr', None, False),    # each leaf through a primitive array
    'O': ('opt', None, False),    # each leaf through Option / Some / List
    'H': ('meth', None, False),   # each leaf through a method with a primitive parameter
    'I': ('tc', None, False),     # same-type arithmetic and comparisons through Numeric / Integral / Fractional / Ordering
    'Q': ('asg', None, False),    # binary operators as compound assignment on a local var
    'D': ('lit', None, False),    # whole expression as a default argument
    'N': ('lit', None, False),    # whole expression as a Java annotation argument
}
R1 = 'ABCLFVWXYZ'

PRELUDE = '''
object Rt {
  @noinline def id[T](x: T): T = x
  def show(x: Any): String = x match {
    case null => "null"
    case v: Byte => "B" + v
    case v: Short => "S" + v
    case v: Char => "C" + v.toInt
    case v: Int => "I" + v
    case v: Long => "J" + v
    case v: Float => "F" + java.lang.Integer.toHexString(java.lang.Float.floatToRawIntBits(v))
    case v: Double => "D" + java.lang.Long.toHexString(java.lang.Double.doubleToRawLongBits(v))
    case v: Boolean => "Z" + v
    case v: String => "T" + v.length + ":" + v.map(c => if (c < 32 || c > 126) "\\\\u%04x".format(c.toInt) else c.toString).mkString
    case v: Unit => "U"
    case v => "?" + v.getClass.getName
  }
  def ex(e: Throwable): String = "!" + e.getClass.getName
}
''' + ''.join('class V%s(val x: %s) extends AnyVal\ncase class W%s(x: %s) extends AnyVal\n' % (t, TNAME[t], t, TNAME[t])
              for t in NUM + ['Z', 'T'])
ANN_JAVA = '''import java.lang.annotation.*;
@Retention(RetentionPolicy.RUNTIME)
public @interface Ann {
  byte b() default 0; short s() default 0; char c() default 0; int i() default 0; long j() default 0;
  float f() default 0; double d() default 0; boolean z() default false; String t() default "";
}
'''


def emit(exprs, variants, objname='Main', chunk=40, skip=()):
    """exprs: list of (id, tree). One method per (id, variant), each on one line."""
    lines = ['import Rt._']
    kdecls, sepdecls, linemap = [], [], {}
    body = []
    for eid, tree in exprs:
        for v in variants:
            if (eid, v) in skip:
                continue
            leaf, box, asinst = VARIANTS[v]
            c = Ctx(leaf, box, asinst, 'x%d%s' % (eid, v))
            s = render(tree, c)
            decls = ''
            if v in 'DN' and (tail(tree).kind in ('if', 'match') or tree.ty in ('U', 'N')):
                continue
            if v == 'D':
                decls = 'def d%d(p: %s = %s): %s = p; ' % (eid, TNAME[tree.ty], s, TNAME[tree.ty])
                s = 'd%d()' % eid
            if v == 'N':
                decls = '@Ann(%s = %s) class An%d; ' % (tree.ty.lower(), s, eid)
                s = 'classOf[An%d].getAnnotation(classOf[Ann]).%s()' % (eid, tree.ty.lower())
            if leaf == 'fval':
                kdecls.extend(c.decls)
            elif leaf == 'sep':
                sepdecls.extend(c.decls)
            else:
                decls += ''.join(d + '; ' for d in c.decls)
            loc = ''.join(d + '; ' for d in c.locals)
            body.append((eid, v, '%sdef e%d%s(): String = try { %sshow(%s) } catch { case e: Throwable => ex(e) }' % (
                decls, eid, v, loc, s)))
    out = [PRELUDE, 'import Rt._', 'object K {']
    out += ['  ' + d for d in kdecls]
    out.append('}')
    nobj = 0
    calls = []
    for i in range(0, len(body), chunk):
        out.append('object %s_%d {' % (objname, nobj))
        for eid, v, m in body[i:i + chunk]:
            out.append('  ' + m)
            calls.append('println("%d %s " + %s_%d.e%d%s())' % (eid, v, objname, nobj, eid, v))
        out.append('}')
        nobj += 1
    # main split into chunks to stay under the method size limit
    out.append('object %s {' % objname)
    nm = 0
    for i in range(0, len(calls), 200):
        out.append('  def m%d(): Unit = {' % nm)
        out += ['    ' + cl for cl in calls[i:i + 200]]
        out.append('  }')
        nm += 1
    out.append('  def main(args: Array[String]): Unit = {')
    out += ['    m%d()' % j for j in range(nm)]
    out.append('  }')
    out.append('}')
    src = '\n'.join(out) + '\n'
    for ln, text in enumerate(src.split('\n'), 1):
        m = re.search(r'def e(\d+)([A-Z])\(\)', text)
        if m:
            linemap[ln] = (int(m.group(1)), m.group(2))
    sep = 'object KSep {\n' + ''.join('  ' + d + '\n' for d in sepdecls) + '}\n'
    return src, linemap, sep


# ---------------------------------------------------------------- compile and run

OPT = {'noopt': [], 'opt': ['-opt:inline:**', '-opt:local']}


def compile_run(src, workdir, build, opt, linemap, sep, timeout=230):
    """returns (results {(eid, variant): value}, rejected {eid: msg}, error)"""
    os.makedirs(workdir, exist_ok=True)
    f = os.path.join(workdir, 'Main.scala')
    out = os.path.join(workdir, 'out-' + opt)
    if os.path.isdir(out):
        shutil.rmtree(out)
    os.makedirs(out)
    open(f, 'w').write(src)
    b = BUILDS[build]
    env = ENV if not JACOCO else dict(ENV, JAVA_OPTS='-Xmx2g -javaagent:%s=destfile=%s,includes=scala.tools.nsc.*,inclbootstrapclasses=true,append=true' % (AGENT, JACOCO))
    fk, fa = os.path.join(workdir, 'KSep.scala'), os.path.join(workdir, 'Ann.java')
    open(fk, 'w').write(sep)
    open(fa, 'w').write(ANN_JAVA)
    try:
        # the constants unit and the annotation are compiled first, so Main reads them from class files
        p0 = subprocess.run(['javac', '-d', out, fa], capture_output=True, text=True, env=ENV, timeout=timeout)
        p0 = subprocess.run([b + '/bin/scalac', '-nowarn', '-d', out] + OPT[opt] + [fk], capture_output=True,
                            text=True, env=ENV, timeout=timeout)
        if p0.returncode != 0:
            return {}, {}, 'KSep: ' + (p0.stdout + p0.stderr)[-3000:]
        p = subprocess.run([b + '/bin/scalac', '-nowarn', '-Xmaxerrs', '100000', '-cp', out, '-d', out] + OPT[opt] + [f],
                           capture_output=True, text=True, env=env, timeout=timeout)
    except subprocess.TimeoutExpired:
        return {}, {}, 'compile timeout'
    if p.returncode != 0:
        rej = {}
        for m in re.finditer(r'Main\.scala:(\d+): error: (.*)', p.stdout + p.stderr):
            ln = int(m.group(1))
            if ln in linemap:
                rej.setdefault(linemap[ln], m.group(2))
        return {}, rej, (p.stdout + p.stderr)[-3000:]
    try:
        r = subprocess.run(['java', '-Xmx1g', '-Xss4m', '-cp', b + '/lib/scala-library.jar:' + out, 'Main'],
                           capture_output=True, text=True, env=ENV, timeout=120)
    except subprocess.TimeoutExpired:
        return {}, {}, 'run timeout'
    res = {}
    for line in r.stdout.split('\n'):
        m = re.match(r'(\d+) ([A-Z]) (.*)', line)
        if m:
            res[(int(m.group(1)), m.group(2))] = m.group(3)
    if r.returncode != 0:
        return res, {}, 'run failed: ' + r.stderr[-2000:]
    return res, {}, None


def evaluate(exprs, variants, workdir, build, opts=('noopt', 'opt')):
    """returns {eid: {(variant, opt): value}}, rejects {eid: (variant, msg)}"""
    exprs = list(exprs)
    rejects = {}   # (eid, variant) -> message; a rejected form is left out, the other forms still run
    table = {}
    for opt in opts:
        for attempt in range(8):
            src, linemap, sep = emit(exprs, variants, skip=rejects)
            res, rej, err = compile_run(src, workdir, build, opt, linemap, sep)
            if rej:
                rejects.update(rej)
                continue
            if err:
                print('ERROR', opt, err, file=sys.stderr)
            for (eid, v), val in res.items():
                table.setdefault(eid, {})[(v, opt)] = val
            break
    for (eid, v) in rejects:
        if eid in table:
            for o in opts:
                table[eid].pop((v, o), None)
    return table, rejects


def disagreements(row):
    """row: {(variant, opt): value}. Returns list of groups if more than one distinct value."""
    by = {}
    for k, v in row.items():
        by.setdefault(v, []).append(k)
    return by if len(by) > 1 else None


def signature(by):
    """partition of variants, opt-insensitive unless opt matters"""
    groups = sorted(''.join(sorted(set(v + ('' if o == 'noopt' else "'") for v, o in ks))) for ks in by.values())
    kinds = sorted(set(v[0] if v else '' for v in by.keys()))
    return '|'.join(groups) + ' ' + ''.join(kinds)


# ---------------------------------------------------------------- shrinking

def subtrees(n, path=()):
    yield path, n
    for i, k in enumerate(n.kids):
        yield from subtrees(k, path + (i,))


def replace(n, path, new):
    if not path:
        return new
    c = N(n.kind, n.ty, n.kids, n.data)
    c.kids[path[0]] = replace(n.kids[path[0]], path[1:], new)
    return c


def leaves_of(n):
    return [x for _, x in subtrees(n) if x.kind == 'lit']


def candidates(tree, rng):
    out = []
    for path, sub in subtrees(tree):
        if sub.kind == 'lit':
            continue
        # hoist a descendant of the same type
        for _, d in subtrees(sub):
            if d is not sub and d.ty == sub.ty:
                out.append(replace(tree, path, d))
        # replace with one of its own leaves of the same type, else a fresh simple leaf
        ls = [l for l in leaves_of(sub) if l.ty == sub.ty]
        if ls:
            out.append(replace(tree, path, ls[0]))
        out.append(replace(tree, path, lit(rng, sub.ty)))
    seen, uniq = set(), []
    for t in out:
        s = render(t, Ctx('lit', None, False, 'x'))
        if s not in seen and t.size() < tree.size():
            seen.add(s)
            uniq.append(t)
    uniq.sort(key=lambda t: t.size())
    return uniq[:150]


def shrink(tree, variants, workdir, build, rng, rounds=12):
    cur = tree
    for _ in range(rounds):
        cands = candidates(cur, rng)
        if not cands:
            break
        table, _ = evaluate(list(enumerate(cands)), variants, workdir, build)
        failing = [cands[i] for i in sorted(table) if disagreements(table[i])]
        if not failing:
            break
        cur = min(failing, key=lambda t: t.size())
    return cur


# ---------------------------------------------------------------- main

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--seed', type=int, default=1)
    ap.add_argument('--n', type=int, default=300)
    ap.add_argument('--batches', type=int, default=1)
    ap.add_argument('--depth', type=int, default=4)
    ap.add_argument('--build', default='int')
    ap.add_argument('--work', required=True)
    ap.add_argument('--variants', default=R1)
    ap.add_argument('--evolved', action='store_true')
    ap.add_argument('--jacoco', help='exec file; attaches the JaCoCo agent to the Main compilation')
    ap.add_argument('--budget', type=int, default=200)
    ap.add_argument('--no-shrink', action='store_true')
    ap.add_argument('--max-shrink', type=int, default=6)
    a = ap.parse_args()
    global EVOLVED, JACOCO
    JACOCO = a.jacoco
    EVOLVED = a.evolved
    variants = list(''.join(VARIANTS) if a.evolved and a.variants == R1 else a.variants)
    import time
    t0 = time.time()
    sigs = {}
    total = rejected = failed = 0
    for b in range(a.batches):
        if time.time() - t0 > a.budget:
            break
        rng = random.Random(a.seed * 1000 + b)
        exprs = []
        for i in range(a.n):
            ty = rng.choice(['I', 'I', 'J', 'F', 'D', 'Z', 'Z', 'T', 'T', 'B', 'S', 'C'])
            exprs.append((i, gen(rng, ty, rng.randint(1, a.depth))))
        table, rejects = evaluate(exprs, variants, os.path.join(a.work, 'b%d' % b), a.build)
        total += len(exprs)
        rejected += len(rejects)
        trees = dict(exprs)
        for (eid, v), msg in rejects.items():
            if v == 'N':
                continue  # expected: the expression is not a constant
            sigs.setdefault('REJECT %s %s' % (v, re.sub(r'\d+', 'N', msg)[:80]), []).append((a.seed, b, eid, trees[eid], None))
        for eid in sorted(table):
            by = disagreements(table[eid])
            if by:
                failed += 1
                sigs.setdefault(signature(by), []).append((a.seed, b, eid, trees[eid], by))
    print('cases %d rejected %d disagreeing %d signatures %d' % (total, rejected, failed, len(sigs)))
    rng = random.Random(a.seed)
    nshrunk = 0
    for sig, items in sorted(sigs.items(), key=lambda kv: -len(kv[1])):
        seed, b, eid, tree, by = items[0]
        print('\n== %s  (%d cases)' % (sig, len(items)))
        print('  first: seed %d batch %d id %d size %d' % (seed, b, eid, tree.size()))
        if by is None:
            v = sig.split()[1]
            c = Ctx(*VARIANTS[v], 'x')
            s = render(tree, c)
            print('  %s: %s%s%s' % (v, ''.join(d + '; ' for d in c.decls), ''.join(d + '; ' for d in c.locals), s))
            continue
        if not a.no_shrink and nshrunk < a.max_shrink:
            nshrunk += 1
            small = shrink(tree, variants, os.path.join(a.work, 'shrink'), a.build, rng)
            table, _ = evaluate([(0, small)], variants, os.path.join(a.work, 'shrink'), a.build)
            by2 = disagreements(table.get(0, {})) or by
            tree, by = small, by2
            print('  shrunk to size %d' % tree.size())
        for v in variants:
            c = Ctx(*VARIANTS[v], 'x')
            s = render(tree, c)
            print('  %s: %s%s%s' % (v, ''.join(d + '; ' for d in c.decls), ''.join(d + '; ' for d in c.locals), s))
        for val, ks in by.items():
            print('  %-40s <- %s' % (val, ' '.join(sorted(v + ('' if o == 'noopt' else "'") for v, o in ks))))


if __name__ == '__main__':
    main()
