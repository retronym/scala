#!/usr/bin/env python3
"""Generates Java sources (JDK 21 level) and a Scala probe per case. usage: gen.py <outdir> <cases> <seed> [features]
features: comma separated subset of K,E,R,S,An,G,I,B,M (default all)."""
import os, random, sys

R = random.Random()

def ch(*xs): return R.choice(xs)
def p(x): return R.random() < x

# ---------------------------------------------------------------- trivia
def ws():
    if p(0.8): return " "
    return ch(" /* c */ ", " /* } { */ ", " // x { \" '\n  ", "\n", " /** d */ ", "\t", " /* \" */ ", " /* ' */ ", " /**/ ")

def mods(*ms, annots=True):
    ms = [m for m in ms if m]
    R.shuffle(ms)
    if annots and p(0.15): ms.insert(R.randrange(len(ms) + 1), ch("@Deprecated", "@SuppressWarnings(\"all\")", "@java.lang.Deprecated", "@SuppressWarnings({\"a\", \"b\"})", "@Deprecated(since = \"12\", forRemoval = false)"))
    return ws().join(ms) + (" " if ms else "")

# ---------------------------------------------------------------- constant expressions
INTS = ["0", "1", "2", "7", "42", "255", "0x7f", "0xFF", "0XcafeBabe", "017", "0777", "0b101", "0B1111_0000", "1_000", "2147483647", "0x8000_0000", "0x7fff_ffff", "037777777777", "0_7", "65536", "31", "32", "33", "-1", "-2147483648", "0xffffffff", "'a'", "'\\n'", "(byte) 200", "(short) 70000", "(char) -1", "3", "5", "100", "1 << 31", "0b1"]
LONGS = ["0L", "1L", "2l", "0xFFFF_FFFFL", "9223372036854775807L", "-9223372036854775808L", "0x8000000000000000L", "0777L", "0b1L", "1_0L", "4294967296L", "63L", "64L", "65L", "-1L", "0xcafebabeL", "01777777777777777777777L"]  # javac does not fold long >>> long (no ConstantValue), so the flat chains below use int literals
FLOATS = ["0f", "1f", "1.5f", ".5f", "1.f", "1e3f", "1E-3F", "0x1.8p1f", "3.4028235e38f", "1.4e-45f", "0x1p-149f", "1_0.2_5f", "-0f", "16777217f", "0.1f", "1e10f", "0x.8p0f", "100f", "1.0e-5f", "123456789f", "0xAp0f"]
DOUBLES = ["0.0", "1.0", "1.5", ".5", "1.", "1e3", "1E-3", "0x1.8p1", "1.7976931348623157e308", "4.9e-324", "1d", "2D", "1_0.2_5", "-0.0", "0.1", "1e-7", "1e7", "1e21", "0x1p-1074", "9007199254740993.0", "100.0", "1.0E10", "0.001", "0x1.fffffffffffffp1023", "3e0", "0x10p0", "1e-3d"]
CHARS = ["'a'", "'Z'", "'0'", "'\\n'", "'\\t'", "'\\\\'", "'\\''", "'\"'", "'\\0'", "'\\7'", "'\\377'", "'\\u0041'", "'\\uFFFF'", "'\\s'", "'\\b'", "'\\r'", "'\\f'", "'\\uuu0062'", "' '", "'{'", "'}'", "'\\u00e9'", "'/'", "'*'", "'\\12'", "'\\47'"]
STRS = ['""', '"a"', '"hello"', '"a\\nb"', '"q\\"q"', '"\\u0041\\u00e9"', '"\\0\\7\\377\\1234"', '"{ } // /* "', '"\\\\"', '"\\s\\t"', '"x" ', '"\\uuu0062c"', '"\'"', '"\\400"', '"\\08"', '"\\779"',
        '"""\n      text\n        block \\\n      cont "q" \\t\\s\n      """', '"""\n    a\n   b\n    """', '"""\n\t  x  \n\t  y\\n\n\t  z"""', '"""\n      \\"""\n      ""\\"\n      {}""" ', '"""\n    \\u0041\\101\n    """']
BOOLS = ["true", "false"]
NUM = ["byte", "short", "char", "int", "long", "float", "double"]

class Consts:
    def __init__(self): self.byT = {}
    def add(self, t, ref): self.byT.setdefault(t, []).append(ref)
    def get(self, t): return self.byT.get(t, [])

def par(e): return "(" + e + ")"

def gexpr(t, d, cs):
    """A Java constant expression of type t, usable as an operand at any precedence."""
    refs = cs.get(t)
    if refs and p(0.2): return R.choice(refs)
    if d <= 0 or p(0.25):
        if t == "int": return par(ch(*INTS)) if p(0.3) else ch(*INTS[:20])
        if t == "long": return par(ch(*LONGS))
        if t == "float": return par(ch(*FLOATS))
        if t == "double": return par(ch(*DOUBLES))
        if t == "char": return ch(*CHARS)
        if t == "byte": return "((byte) %s)" % ch("1", "-1", "127", "200", "'a'", "0x80", "-129", "1.9", "3e10", "300L")
        if t == "short": return "((short) %s)" % ch("1", "-1", "32767", "70000", "'a'", "0x8000", "-1.5f", "1e10", "65535L")
        if t == "boolean": return ch(*BOOLS)
        if t == "String": return ch(*STRS)
    d -= 1
    if t in ("byte", "short", "char"):
        k = R.randrange(3)
        if k == 0: return "((%s) %s)" % (t, gexpr(ch(*NUM), d, cs))
        if k == 1: return "(%s ? %s : %s)" % (gexpr("boolean", d, cs), gexpr(t, d, cs), gexpr(t, d, cs))
        return "((%s) %s)" % (t, gexpr("int", d, cs))
    if t in ("int", "long"):
        sub = ["byte", "short", "char", "int"] if t == "int" else ["long"]
        k = R.randrange(10)
        if k < 4:
            op = ch("+", "-", "*", "&", "|", "^")
            a, b = gexpr(ch(*sub), d, cs), gexpr(ch(*sub), d, cs) if t == "int" else gexpr(ch("byte", "short", "char", "int", "long"), d, cs)
            if p(0.5): a, b = b, a
            if t == "long" and "L" not in a + b and "l" not in a + b: a = "(long) " + a if p(0.5) else gexpr("long", 0, cs)
            return "(%s %s %s)" % (a, op, b)
        if k == 4:
            dv = ch("1", "2", "3", "-1", "7", "-3", "(byte) 5", "'a'", "0x10", "1000")
            if t == "long": dv = ch(dv, "3L", "-1L", "-7L", "0x100000000L")
            lhs = gexpr(t, d, cs) if t == "long" else gexpr(ch(*sub), d, cs)
            return "(%s %s %s)" % (lhs, ch("/", "%"), dv)
        if k == 5:
            return "(%s %s %s)" % (gexpr(t if t == "long" else ch(*sub), d, cs), ch("<<", ">>", ">>>"), gexpr(ch("int", "int", "byte", "char", "short"), d, cs))
        if k == 6: return "(%s%s)" % (ch("-", "~", "+", "- ", "~ ~", "- -", "+ +", "-~"), gexpr(t if t == "long" else ch(*sub), d, cs))
        if k == 7: return "((%s) %s)" % (t, gexpr(ch(*NUM), d, cs))
        if k == 8: return "(%s ? %s : %s)" % (gexpr("boolean", d, cs), gexpr(t, d, cs), gexpr(t if t == "long" else ch(*sub), d, cs))
        # flat chain without parentheses: exercises precedence, type stays t
        n = R.randrange(2, 6)
        atoms = [gexpr(t, 0, cs)] + [gexpr("int", 0, cs) for _ in range(n - 1)]
        s = atoms[0]
        for a in atoms[1:]: s += ws() + ch("+", "-", "*", "&", "|", "^", "<<", ">>", ">>>") + ws() + a
        return par(s)
    if t in ("float", "double"):
        k = R.randrange(6)
        other = ["byte", "short", "char", "int", "long", "float"] + (["double"] if t == "double" else [])
        if k < 3:
            a, b = gexpr(t, d, cs), gexpr(ch(*other), d, cs)
            if p(0.5): a, b = b, a
            return "(%s %s %s)" % (a, ch("+", "-", "*", "/", "%"), b)
        if k == 3: return "(%s%s)" % (ch("-", "+", "- -"), gexpr(t, d, cs))
        if k == 4: return "((%s) %s)" % (t, gexpr(ch(*NUM), d, cs))
        return "(%s ? %s : %s)" % (gexpr("boolean", d, cs), gexpr(t, d, cs), gexpr(ch(*other), d, cs))
    if t == "boolean":
        k = R.randrange(8)
        if k < 3:
            a, b = ch(*NUM), ch(*NUM)
            return "(%s %s %s)" % (gexpr(a, d, cs), ch("<", "<=", ">", ">=", "==", "!="), gexpr(b, d, cs))
        if k == 3: return "(!%s)" % gexpr("boolean", d, cs)
        if k == 4: return "(%s %s %s)" % (gexpr("boolean", d, cs), ch("&&", "||", "&", "|", "^", "==", "!="), gexpr("boolean", d, cs))
        if k == 5: return "(%s %s %s)" % (gexpr("String", d, cs), ch("==", "!="), gexpr("String", d, cs))
        if k == 6: return "(%s ? %s : %s)" % (gexpr("boolean", d, cs), gexpr("boolean", d, cs), gexpr("boolean", d, cs))
        n = R.randrange(2, 5)
        s = gexpr("boolean", 0, cs)
        for _ in range(n): s += " " + ch("&&", "||", "&", "|", "^", "==", "!=") + " " + ch("", "!") + gexpr("boolean", 0, cs)
        return par(s)
    if t == "String":
        k = R.randrange(6)
        anyT = ch(*NUM, "boolean", "String", "String")
        if k < 2:
            a, b = gexpr("String", d, cs), gexpr(anyT, d, cs)
            if p(0.5): a, b = b, a
            return "(%s + %s)" % (a, b)
        if k == 2: return "((%s) %s)" % (ch("String", "java.lang.String"), gexpr("String", d, cs))
        if k == 3: return "(%s ? %s : %s)" % (gexpr("boolean", d, cs), gexpr("String", d, cs), gexpr("String", d, cs))
        if k == 4:  # mixed ternary whose type shows through string conversion
            a, b = ch(*NUM), ch(*NUM)
            return '("" + (%s ? %s : %s))' % (gexpr("boolean", d, cs), gexpr(a, d, cs), gexpr(b, d, cs))
        n = R.randrange(2, 6)
        parts = [gexpr(ch(*NUM, "boolean", "String"), 0, cs) for _ in range(n)]
        parts.insert(R.randrange(2), gexpr("String", 0, cs))
        return par(" + ".join(parts))

ASSIGN = {"long": ["long", "int", "char", "short", "byte"], "double": ["double", "float", "long", "int", "char"], "float": ["float", "long", "int", "char", "byte"],
          "int": ["int", "char", "short", "byte"], "short": ["short", "byte"], "byte": ["byte"], "char": ["char"], "boolean": ["boolean"], "String": ["String"]}
NARROW = {"byte": ["10", "'a'", "-128", "127", "(short) 5", "0x7f", "1 + 2", "true ? 1 : 2", "'\\0'"], "short": ["'a'", "32767", "-1", "(byte) 3 + 1", "0x7fff"], "char": ["65", "0xFFFF", "(byte) 7", "(short) 9", "1 + 'a'", "0"]}
NONCONST = [("int", "Integer.parseInt(\"3\")"), ("Integer", "1"), ("String", "null"), ("int", "\"abc\".length()"), ("Object", "\"s\""), ("CharSequence", "\"s\""), ("int", "(Integer) 5"),
            ("int", "Math.max(1, 2)"), ("String", "String.valueOf(1)"), ("boolean", "\"a\" instanceof String"), ("int[]", "{1, 2}"), ("String", "\"a\" + null"), ("int", "new int[]{1, 2}.length"),
            ("long", "Long.MAX_VALUE"), ("int", "Integer.MIN_VALUE"), ("String", "(\"a\").toString()"), ("double", "Math.PI"), ("char", "Character.MAX_VALUE"), ("int", "java.lang.Integer.MAX_VALUE + 1"),
            ("float", "Float.NaN"), ("double", "Double.MIN_VALUE"), ("String", "(String) (Object) \"s\""), ("int", "1 + (int) (Object) 1"), ("String", "\"a\".concat(\"b\")"), ("boolean", "Boolean.TRUE")]

def gen_K(pkg, cs, name, iface):
    lines, probe = [], []
    n = R.randrange(6, 14)
    for i in range(n):
        fname = ch("C", "Kc", "x", "_c", "$d", "VAL") + str(i)
        static = iface or p(0.85)
        if p(0.12):
            t, init = R.choice(NONCONST)
            decl_t = t
        else:
            t = ch("boolean", "byte", "short", "char", "int", "long", "float", "double", "String", "String", "int", "long")
            if t in NARROW and p(0.3): init = ch(*NARROW[t])
            else:
                init = gexpr(ch(*ASSIGN[t]), R.randrange(0, 4), cs)
                if p(0.5) and init.startswith("(") and init.endswith(")") and init.count("(") == 1: init = init[1:-1]
            decl_t = ch("String", "String", "java.lang.String") if t == "String" else t
            if static: cs.add(t, name + "." + fname)
        if iface: m = mods(*R.sample(["public", "static", "final"], R.randrange(4)))
        else: m = mods("public", "static" if static else "", "final")
        arr = "[]" if decl_t == "int[]" and p(0.5) else ""
        if arr: decl_t = "int"
        lines.append("  %s%s%s%s%s%s=%s%s;" % (m, decl_t, ws(), fname, arr, ws(), ws(), init))
        if static: probe.append('pr("%s.%s", %s.%s)' % (name, fname, name, fname))
        else: probe.append('pr("%s#%s", new %s().%s)' % (name, fname, name, fname))
    R.shuffle(lines)
    kw = "interface" if iface else "class"
    return {name: "public %s %s {\n%s\n}\n" % (kw, name, "\n".join(lines))}, probe

# ---------------------------------------------------------------- other features: templates with variation
def gen_E(pkg, cs):
    files, probe = {}, []
    names = ["A", "B", "C", "D"][:R.randrange(1, 5)]
    abstract = p(0.3)
    consts = []
    for i, n in enumerate(names):
        c = (ch("", "@Deprecated ", "/** doc */ ", "@SuppressWarnings(\"x\") ")) + n
        k = R.randrange(5)
        if k == 1: c += "()"
        elif k == 2: c += "(%d)" % i
        elif k == 3: c += '("s,)(", %s)' % ch("1", "(int) 2L", "new int[]{1,2}.length", "java.util.List.<String>of().size()", "'}' + 1")
        if abstract or p(0.3):
            c += " { " + ("@Override public int g() { return %d; } " % i if abstract else "") + ch("", "public void extra() { String s = \"}\"; char c = '{'; }", "int f = 1; { f++; }", "static int q = 2;") + " }"
        consts.append(c)
    end = ch(";", ",;", ";;") if names else ch(";", "")
    body = [
        "  private final int n; private String s;",
        "  E() { this(0); }", "  E(int n) { this.n = n; }", "  private E(String s, int n) { this.n = n; this.s = s; }",
        "  public int f() { return n; }",
        "  public abstract int g();" if abstract else "  public int g() { return -1; }",
        "  public static final int COUNT = %s;" % gexpr("int", 2, cs),
        "  public static E of(int i) { return values()[i % values().length]; }",
        "  public enum Nested { X, Y%s }" % ch("", ";", ",", ",;"),
        "  public interface Op { int ap(int x); }",
        "  static { int z = 0; }",
        "  public final String label = \"L\";",
    ]
    R.shuffle(body)
    impl = ch("", " implements Runnable", " implements java.io.Serializable, Comparable<E>" if False else " implements java.util.function.IntSupplier")
    extra = {" implements Runnable": "  public void run() {}", " implements java.util.function.IntSupplier": "  public int getAsInt() { return 5; }", "": ""}[impl]
    files["E"] = "public%senum E%s {\n  %s%s\n%s\n%s\n}\n" % (ws(), impl, ", ".join(consts), end, "\n".join(body), extra)
    probe += ['E.values.foreach(e => pr("E", e.toString + " " + e.f + " " + e.g + " " + e.ordinal + " " + e.getDeclaringClass + " " + e.label))',
              'pr("E.COUNT", E.COUNT)', 'pr("E.of", E.of(1))', 'pr("E.valueOf", E.valueOf("A"))', 'pr("Nested", E.Nested.values.toList)',
              'def m(e: E): Int = e match { %s }' % "; ".join("case E.%s => %d" % (n, i) for i, n in enumerate(names[:len(names) - (1 if p(0.5) and len(names) > 1 else 0)])),
              'pr("m", scala.util.Try(E.values.map(m).toList))', 'val op: E.Op = x => x + 1', 'pr("op", op.ap(1))']
    if p(0.5):
        files["E0"] = "public enum E0 %s\n" % ch("{}", "{;}", "{ ; public static int z() { return 1; } }", "{ }")
        probe.append('pr("E0", E0.values.length)')
    return files, probe

def gen_R(pkg, cs):
    files, probe = {}, []
    generic = p(0.5)
    varargs = p(0.5)
    tp = ch("<T>", "<T extends Comparable<T>>", "<T extends CharSequence & java.io.Serializable>") if generic else ""
    comps = ["int x", ch("String s", "@Deprecated String s", "final String s" if False else "java.lang.String s"), ("T t" if generic else "long[] t")]
    if varargs: comps.append(ch("int... rest", "int @Deprecated ... rest" if False else "int ... rest"))
    members = [
        "  public static final int Z = %s;" % gexpr("int", 2, cs),
        ch("  public R { if (x < 0) throw new IllegalArgumentException(\"}\"); }", "  R { }" if False else "  public R { x = x + 0; }", ""),
        "  public R(int x) { this(x, \"d\", null%s); }" % (", 1, 2" if varargs else ""),
        ch("  public int x() { return x + 1; }", "  @Override public int x() { return x; }", ""),
        "  public static int st() { return 7; }",
        "  public record Inner(double d, long[] ls, java.util.List<? extends Number> l) %s" % ch("{}", "{ public Inner { } }", "{ public Inner(double d, long[] ls, java.util.List<? extends Number> l) { this.d = d; this.ls = ls; this.l = l; } }", "implements Runnable { public void run() {} }"),
        "  int twice() { return x * 2; }",
        "  public interface RI { record Deep(String... ss) {} }",
        "  public record Empty() {}",
        "  public enum RE { P, Q }",
    ]
    R.shuffle(members)
    files["R"] = "public record R%s(%s)%s {\n%s\n}\n" % (tp, ", ".join(comps), ch("", " implements java.io.Serializable"), "\n".join(members))
    ta = "[String]" if generic else ""
    targ = '"t"' if generic else "Array(1L)"
    probe += ['val r = new R%s(1, "s", %s%s)' % (ta, targ, ", 3, 4" if varargs else ""), 'pr("r.x", r.x)', 'pr("r.s", r.s)', 'pr("r", r.toString.take(12))', 'pr("r1", new R%s(5).x)' % ta,
              'pr("R.Z", R.Z)', 'pr("R.st", R.st())', 'pr("inner", new R.Inner(1.5, Array(1L), null).d)', 'pr("deep", new R.RI.Deep("a", "b").ss.length)', 'pr("empty", new R.Empty())', 'pr("re", R.RE.Q)', 'pr("tw", r.twice())']
    if varargs: probe.append('pr("rest", r.rest.length)')
    return files, probe

def gen_S(pkg, cs):
    files, probe = {}, []
    k = R.randrange(4)
    if k == 0:
        files["S"] = "public sealed interface S permits S.A, S.B, SC {\n  record A(int x) implements S {}\n  final class B implements S {}\n  default int v() { return 1; }\n}\n"
        files["SC"] = "public %s class SC implements S {}\n" % ch("final", "non-sealed")
        subs = ["S.A", "S.B", "SC"]; mk = ["new S.A(1)", "new S.B", "new SC"]
    elif k == 1:
        files["S"] = "public abstract sealed class S {\n  public static final class A extends S {}\n  public static non-sealed class B extends S {}\n  public int v() { return 2; }\n  static final class Hidden extends S {}\n}\n"
        subs = ["S.A", "S.B"]; mk = ["new S.A", "new S.B"]
    elif k == 2:
        files["S"] = "public sealed interface S {\n  enum A implements S { I, J }\n  record B(S inner) implements S {}\n  sealed interface C extends S permits D {}\n  record D() implements C {}\n  default int v() { return 3; }\n}\n"
        subs = ["S.A", "S.B", "S.C"]; mk = ["S.A.I", "new S.B(null)", "new S.D"]
    else:
        files["S"] = "public sealed class S permits SC, SD {\n  public int v() { return 4; }\n}\n"
        files["SC"] = "public final class SC extends S {}\n"
        files["SD"] = "public sealed class SD extends S permits SD.E {\n  public static final class E extends SD {}\n}\n"
        subs = ["SC", "SD"]; mk = ["new SC", "new SD", "new SD.E", "new S"]
    drop = 1 if p(0.5) else 0
    cases = "; ".join("case _: %s => %d" % (s, i) for i, s in enumerate(subs[:len(subs) - drop]))
    probe += ['def sm(s: S): Int = s match { %s }' % cases, 'pr("sm", List[S](%s).map(s => scala.util.Try(sm(s)).isSuccess + " " + s.v))' % ", ".join(mk)]
    return files, probe

def gen_An(pkg, cs):
    files, probe = {}, []
    elems = [
        "int i() default %s;" % gexpr("int", 2, cs), "long l() default %s;" % gexpr("long", 1, cs), "byte b() default 1;", "short sh() default (short) 2;",
        "char c() default %s;" % ch(*CHARS), "float f() default %s;" % gexpr("float", 1, cs), "double d() default %s;" % gexpr("double", 1, cs), "boolean z() default %s;" % gexpr("boolean", 2, cs),
        "String s() default %s;" % gexpr("String", 2, cs), "Class<?> k() default %s;" % ch("Object.class", "int.class", "void.class", "String[].class", "java.util.Map.Entry.class", "An.class"),
        "Mode e() default Mode.X;", "Nested n() default @Nested(v = 1);", "int[] is() default {1, 2};", "String[] ss() default {};", "int[] one() default 3;",
        "Class<? extends Number>[] ks() default {Integer.class, Long.class,};", "Nested[] ns() default {@Nested, @Nested(v = 2)};", "Mode[] es() default Mode.Y;",
        "int arr()[] default {};", "public abstract String pa() default \"p\";", "int CONST = %s;" % gexpr("int", 2, cs), "String SC = %s;" % gexpr("String", 1, cs),
        "@interface Nested { int v() default 0; }", "enum Mode { X, Y }", "class Helper { public static int h() { return 1; } }",
    ]
    R.shuffle(elems)
    req = p(0.5)
    if req: elems.append("int value();")
    else: elems.append("int value() default -1;")
    ret = "@java.lang.annotation.Retention(java.lang.annotation.RetentionPolicy.RUNTIME)"
    tgt = ch("", "@java.lang.annotation.Target({java.lang.annotation.ElementType.TYPE, java.lang.annotation.ElementType.METHOD})", "@java.lang.annotation.Documented", "@java.lang.annotation.Inherited")
    files["An"] = "%s\n%s\npublic @interface An {\n  %s\n}\n" % (ret, tgt, "\n  ".join(elems))
    probe += ['@An(%s) class Ann1' % ch("1", "value = 1", "value = 2, i = 3", 'value = 1, s = "q", is = Array(1, 2), e = An.Mode.Y, k = classOf[String]', "value = 1, n = new An.Nested(v = 3), ns = Array(new An.Nested), es = Array(An.Mode.X)", "value = 1, c = 'x', b = 2, sh = 3, f = 1f, d = 2.0, l = 5L, z = false, one = Array(9), ss = Array(\"a\")"),
              'pr("ann1", classOf[Ann1].getAnnotation(classOf[An]))', 'pr("An.CONST", An.CONST)', 'pr("An.SC", An.SC)', 'pr("An.h", An.Helper.h())']
    if not req: probe += ['@An class Ann2', 'pr("ann2", classOf[Ann2].getAnnotation(classOf[An]))']
    return files, probe

def gen_G(pkg, cs):
    files, probe = {}, []
    b1 = ch("T", "T extends Comparable<T>", "T extends Comparable<T> & java.io.Serializable", "T extends CharSequence", "T extends Object & Comparable<T>")
    b2 = ch("U", "U extends java.util.List<? extends T>", "U extends java.util.Collection<T>", "U extends Iterable<? super T>")
    sup = ch("", " extends java.util.AbstractList<T>", " implements Cloneable, java.io.Serializable", " extends Number")
    supbody = {"": "", " extends java.util.AbstractList<T>": "  public T get(int i) { return null; }\n  public int size() { return 0; }", " implements Cloneable, java.io.Serializable": "",
               " extends Number": "  public int intValue() { return 1; }\n  public long longValue() { return 1; }\n  public float floatValue() { return 1; }\n  public double doubleValue() { return 1; }"}[sup]
    members = [
        "  public T t; public U[] us; public java.util.Map<String, ? super Integer>[] maps[];",
        "  public G() {}", "  public <X extends Throwable> G(X x) throws X {}",
        "  public <A extends Number & Comparable<A>> A max(A a, A b) { return a.compareTo(b) > 0 ? a : b; }",
        "  @SafeVarargs public static <A> java.util.List<A> listOf(A... as) { return java.util.Arrays.asList(as); }",
        "  public class Inner<V> { public V v; public T outerT() { return t; } public class Deep { public V dv() { return v; } } }",
        "  public static class SNested<W> extends G<String, java.util.List<String>> { public W w; }",
        "  public G<T, U>.Inner<String> mkInner() { return new Inner<String>(); }",
        "  public int[] arr()[] { return new int[][]{{1}}; }",
        "  public java.util.List<?>[] wild(java.util.List<? extends int[]> x, int[]... y) { return null; }",
        "  protected int prot() { return 1; } int pkgp() { return 2; } protected static int sprot = 3;",
        "  public static <K extends Enum<K>> K en(Class<K> c) { return c.getEnumConstants()[0]; }",
        "  public <Q extends T> java.util.Map.Entry<? super Q, ? extends U> ent() { return null; }",
        "  public java.util.function.Function<? super T, ? extends java.util.List<? extends U>> fn;",
        "  public static int cnt(java.util.List<? extends Number>... ls) { return ls.length; }",
        "  public interface Cb<P, Q> { Q call(P p) throws Exception; }",
        "  public static <P> P run(Cb<? super String, ? extends P> cb) throws Exception { return cb.call(\"z\"); }",
        "  public T[] ts(T... xs) { return xs; }",
        "  public static final java.util.List<String> EMPTY = java.util.Collections.<String>emptyList(), E2 = null;",
        "  public <Z> G(Z z, int i) { this(); }",
    ]
    R.shuffle(members)
    files["G"] = "public class G<%s, %s>%s {\n%s\n%s\n}\n" % (b1, b2, sup, "\n".join(members), supbody)
    probe += ['val g = new G[String, java.util.List[String]]', 'pr("max", g.max[Integer](1, 2))', 'pr("listOf", G.listOf(1, 2, 3))', 'val gi = new g.Inner[String]', 'pr("inner", gi.outerT)',
              'pr("deep", new gi.Deep().dv)', 'pr("sn", new G.SNested[Int].w)', 'pr("mk", g.mkInner().v)', 'pr("arr", g.arr()(0)(0))', 'pr("wild", g.wild(null, Array(1), Array(2)))',
              'pr("pkgp", g.pkgp)', 'pr("en", G.en(classOf[java.util.concurrent.TimeUnit]))', 'pr("cnt", G.cnt(new java.util.ArrayList[Integer]))', 'pr("run", G.run[Int]((s: String) => s.length))',
              'pr("ts", g.ts("a", "b").length)', 'pr("empty", G.EMPTY)', 'pr("fn", g.fn)', 'pr("maps", g.maps)', 'class GS extends G[String, java.util.List[String]] { def p = prot() + G.sprot }', 'pr("gs", new GS().p)',
              'pr("g2", new G[String, java.util.List[String]]("z", 1).t)']
    return files, probe

def gen_I(pkg, cs):
    files, probe = {}, []
    members = [
        "  int C = %s;" % gexpr("int", 2, cs), "  %sString D = %s;" % (mods("public", "static", "final"), gexpr("String", 1, cs)),
        "  String name();", "  default String hi() { return \"hi\" + priv() + ps(); }", "  static int st() { return 2; }", "  private String priv() { return \"p\"; }", "  private static int ps() { return 1; }",
        "  %sint twice(int x);" % mods("abstract", "public"), "  %sint dflt(int x) { return x; }" % mods("default", "public"), "  %sint pst() { return 3; }" % mods("static", "public"),
        "  class InIface { public int v = 1; }", "  interface Sub<T> extends I { T sub(); }", "  enum IE { U }", "  record IR(int q) {}",
    ]
    R.shuffle(members)
    files["I"] = "public interface I {\n%s\n}\n" % "\n".join(members)
    extra = ch("", "boolean equals(Object o);", "String toString(); int hashCode();", "default int d() { return 1; }", "static int s() { return 1; }", "@Override boolean equals(Object o);")
    sig = ch(("int ap(int a)", "(a: Int) => a + 1", "f.ap(1)"), ("B ap(A a, int... xs) throws java.io.IOException, RuntimeException", None, None), ("<T> T ap(T t)", None, None),
             ("String ap(String a, java.util.List<? extends CharSequence> l)", "(a: String, l: java.util.List[_ <: CharSequence]) => a + l", 'f.ap("a", null)'), ("void ap()", "() => println(\"ran\")", "f.ap()"),
             ("int[] ap(int[] a[])", "(a: Array[Array[Int]]) => a(0)", "f.ap(Array(Array(1)))(0)"), ("long ap(long a, double b, char c)", "(a: Long, b: Double, c: Char) => a + c", "f.ap(1L, 2.0, 'c')"))
    tps = "<A, B extends A>" if "B ap" in sig[0] else ""
    files["F"] = "%spublic interface F%s {\n  %s;\n  %s\n}\n" % (ch("@FunctionalInterface\n", ""), tps, sig[0], extra)
    probe += ['pr("I.C", I.C)', 'pr("I.D", I.D)', 'pr("I.st", I.st() + I.pst())', 'val ii = new I { def name = "n"; def twice(x: Int) = 2 * x }', 'pr("ii", ii.hi + ii.name + ii.twice(2) + ii.dflt(3))',
              'pr("in", new I.InIface().v)', 'pr("ie", I.IE.U)', 'pr("ir", new I.IR(1).q)', 'val sub: I.Sub[String] = new I.Sub[String] { def name = "s"; def twice(x: Int) = x; def sub = "sub" }', 'pr("sub", sub.sub)']
    if sig[1]: probe += ['val f: F = %s' % sig[1], 'pr("f", %s)' % sig[2]]
    return files, probe

# initializers and bodies that the parser must skip; each is followed by a sentinel member
INITS = ["a < b ? 1 : 2", "new java.util.HashMap<String, Integer>().size()", "java.util.List.<String>of(\"a\", \"b\").size()", "(a < 1) ? b >> 2 : b >>> 3", "new int[]{1, 2, 3}.length", "\"},;\".length()", "'}' + ';'",
         "new Object() { int q = 1, r = 2; public int hashCode() { return q; } }.hashCode()", "java.util.stream.Stream.of(1, 2).mapToInt(x -> { return x; }).sum()", "((java.util.function.IntSupplier) () -> 3).getAsInt()",
         "switch (a) { case 1, 2 -> 3; default -> { yield 4; } }", "a > b ? a : b", "f2(a, b)", "(int) (long) a", "a << 2 >> 1", "ident(1 < 2, 3 > 4) ? 1 : 0", "\"\"\"\n      } ; , \" {\n      \"\"\".length()", "a < b && b > a ? 1 : 0",
         "java.util.Map.<String, java.util.List<Integer>>of().size()", "new int[][]{{1, 2}, {3}}[0][1]", "(a) + (b)", "java.util.Objects.hash(a, b, \"x\")", "a /* , ; */ + 1", "a // , ;\n + 1", "a<b?1:b>a?2:3",
         "new java.util.ArrayList<java.util.List<String>>() {{ add(null); }}.size()", "(Integer) 5 instanceof Integer i && i > 2 ? 1 : 0", "Integer.valueOf(1) instanceof Integer ? 1 : 2", "a+++b", "a-- - --b", "'\\'' + '\"' + '\\\\'"]
BODIES = ["{ return 1; }", "{ String s = \"}\"; return s.length(); }", "{ char c = '}'; char d = '\\''; return c + d; }", "{ /* } */ return 1; // }\n }", "{ var x = new java.util.ArrayList<String>(); return x.size(); }",
          "{ Object o = 1; return switch (o) { case Integer i when i > 0 -> i; case String s -> s.length(); default -> 0; }; }", "{ record L(int x) {} enum LE { P } interface LI {} class LC { int y() { return 1; } } return new L(1).x() + new LC().y(); }",
          "{ Object o = \"s\"; if (o instanceof String s && s.length() > 0) { return 1; } return 0; }", "{ int[] a = {1, 2}; int b[] = new int[]{3}; return a[0] + b[0]; }", "{ Runnable r = () -> { }; java.util.function.Function<Integer, Integer> f = (x) -> x + 1; return f.apply(1); }",
          "{ String t = \"\"\"\n      } { \" \\\"\"\"\n      \"\"\"; return t.length(); }", "{ l: for (int i = 0; i < 3; i++) { if (i == 1) continue l; else break l; } return 2; }", "{ try { return 1; } catch (RuntimeException | Error e) { return 2; } finally { } }",
          "{ return new Object() { int v() { return 5; } }.v(); }", "{ int x = 5; x >>>= 1; x >>= 1; x <<= 2; return x > 1 ? x : -x; }", "{ synchronized (this) { return java.util.List.<Integer>of(1).get(0); } }",
          "{ var r = new Object() { int q = 1; }; return r.q; }", "{ int yield = 1; int record = 2; int var = 3; int sealed = 4; int permits = 5; return yield + record + var + sealed + permits; }",
          "{ Object o = new int[0]; return switch (o) { case int[] ia -> 1; case null, default -> 2; }; }", "{ char c = '\"'; String s = \"'\"; return c + s.length(); }", "{ return (a < b) ? a : b > 0 ? 1 : 2; }",
          "{ java.util.function.BiFunction<Integer, Integer, Integer> g = Integer::sum; return g.apply(1, 2); }", "{ assert a > 0 : \"}\"; do { a--; } while (a > 0); return a; }"]

def gen_B(pkg, cs):
    files, probe = {}, []
    members = ["  public int a = 3, b = 4;", "  static int f2(int x, int y) { return x + y; }", "  static boolean ident(boolean x, boolean y) { return x; }"]
    n = R.randrange(5, 12)
    for i in range(n):
        k = R.randrange(6)
        if k == 0:
            members.append("  public int t%d = %s, s%d = %d;" % (i, ch(*INITS), i, i))
            probe.append('pr("s%d", bb.s%d + bb.t%d)' % (i, i, i))
        elif k == 1:
            members.append("  public int t%d() %s" % (i, ch(*BODIES)))
            members.append("  public int s%d() { return %d; }" % (i, i))
            probe.append('pr("s%d", bb.s%d() + bb.t%d())' % (i, i, i))
        elif k == 2:
            members.append("  %s { int z = %s; }" % (ch("", "static") if False else "", ch(*INITS)))
            members.append("  public int s%d = %d;" % (i, i)); probe.append('pr("s%d", bb.s%d)' % (i, i))
        elif k == 3:
            members.append("  public final int t%d = %s, s%d[] = {%d}, u%d = %s;" % (i, ch(*INITS), i, i, i, gexpr("int", 1, cs)))
            probe.append('pr("s%d", bb.s%d(0) + bb.u%d)' % (i, i, i))
        elif k == 4:
            members.append("  public java.util.function.Function<Integer, java.util.Map<String, Integer>> t%d = x -> { return null; }, s%d = null;" % (i, i))
            members.append("  public static final int q%d = %s;" % (i, gexpr("int", 1, cs)))
            probe.append('pr("s%d", "" + bb.s%d + B.q%d)' % (i, i, i))
        else:
            members.append("  public static class N%d { int a = 1, b = 2; public int t = %s; static int f2(int x, int y) { return 0; } static boolean ident(boolean x, boolean y) { return x; } public int s() %s public static final String Q = %s; }" % (i, ch(*INITS), ch(*BODIES), gexpr("String", 1, cs)))
            probe.append('pr("n%d", "" + new B.N%d().s() + B.N%d.Q)' % (i, i, i))
    files["B"] = "public class B {\n%s\n}\n" % "\n".join(members)
    probe.insert(0, "val bb = new B")
    return files, probe

def gen_M(pkg, cs):
    files, probe = {}, []
    members = [
        "  %sint tv;" % mods("public", "transient", "volatile"), "  public int[] a[], b, c[][];", "  %sint[] X[] = {{1}}, Y = {2};" % mods("public", "final", "static"),
        "  %sint f(int[] a[], String... s) throws Exception, java.io.IOException;" % mods("public", "abstract"), "  %sint nat(final int x);" % mods("public", "native"),
        "  %s<T> T g(T t) { return t; }" % mods("protected", "static", "synchronized", "final", "strictfp"), "  public void va(int a, Object... os) {}", "  public int va2(int[]... as) { return as.length; }",
        "  public int va3(final java.util.List<String> @Deprecated ... ls) { return ls.length; }" if False else "  public int va3(final java.util.List<String>... ls) { return ls.length; }",
        "  %sclass MI { public int v = 1; }" % mods("public", "static", "final", "strictfp"), "  %sinterface MJ { int j(); }" % mods("public", "static", "abstract"),
        "  %slong L = %s;" % (mods("public", "static", "final"), gexpr("long", 2, cs)), "  public M() throws Exception {}", "  protected M(int... xs) {}",
        "  public String[] sa()[] { return null; }", "  public <T extends Exception> void thr() throws T, java.lang.Error {}", "  public static void main(String[] args) {}", "  public static void main2(String args[]) {}", "  public static void main3(String... args) {}",
        "  public final int fin() { return 1; }", "  public static double dd(double... ds) { return ds.length; }", "  public void fp(final int a, @Deprecated final String b, final int... c) {}",
        "  public void rcv(M this, int a) {}", "  public java.lang.@Deprecated String qual() { return null; }" if False else "  public java.lang.String qual() { return \"q\"; }", "  public char[] cs = {'a'}, cs2[] = {{'b'}};",
        "  public static final char CH = %s; public static final byte BY = %s;" % (gexpr("char", 2, cs), gexpr("byte", 2, cs)), "  ;", "  public static final short SH = %s;; public static final boolean BO = %s;" % (gexpr("short", 2, cs), gexpr("boolean", 2, cs)),
        "  public static final float FL = %s; public static final double DB = %s;" % (gexpr("float", 2, cs), gexpr("double", 2, cs)),
    ]
    R.shuffle(members)
    files["M"] = "public abstract class M {\n%s\n}\n" % "\n".join(members)
    probe += ['val mm = new M { def f(a: Array[Array[Int]], s: String*) = s.length }', 'pr("f", mm.f(null, "a", "b"))', 'pr("X", M.X(0)(0) + M.Y(0))', 'pr("va2", mm.va2(Array(1), Array(2)))', 'pr("va3", mm.va3(null, null))',
              'pr("mi", new M.MI().v)', 'val mj: M.MJ = () => 4', 'pr("mj", mj.j())', 'pr("L", M.L)', 'pr("dd", M.dd(1, 2, 3))', 'pr("qual", mm.qual)', 'pr("cs", mm.cs(0).toString + mm.cs2(0)(0))',
              'pr("CH", M.CH)', 'pr("BY", M.BY)', 'pr("SH", M.SH)', 'pr("BO", M.BO)', 'pr("FL", M.FL)', 'pr("DB", M.DB)', 'pr("tv", mm.tv + mm.fin())', 'mm.va(1); mm.va(1, "a", "b"); mm.fp(1, "b"); mm.rcv(1); M.main3()', 'pr("abc", "" + mm.a + mm.b + mm.c)']
    return files, probe

PRELUDE = '''object Probe {
  def show(a: Any): String = a match {
    case null => "null"
    case f: Float => "Float:" + java.lang.Float.floatToRawIntBits(f)
    case d: Double => "Double:" + java.lang.Double.doubleToRawLongBits(d)
    case c: Char => "Char:" + c.toInt
    case s: String => "String:" + s.map(_.toInt).mkString(",")
    case x: Array[_] => "Array:" + x.length
    case x: java.lang.annotation.Annotation => x.toString
    case x => x.getClass.getName + ":" + x
  }
  def pr(l: String, a: Any): Unit = println(l + "=" + show(a))
'''

def gen_case(root, idx, feats):
    pkg = "p%05d" % idx
    cs = Consts()
    files, probe = {}, []
    def add(r):
        files.update(r[0]); probe.extend(r[1])
    if "K" in feats:
        add(gen_K(pkg, cs, "IK", True)); add(gen_K(pkg, cs, "K", False)); add(gen_K(pkg, cs, "K2", False))
    for f, g in [("E", gen_E), ("R", gen_R), ("S", gen_S), ("An", gen_An), ("G", gen_G), ("I", gen_I), ("B", gen_B), ("M", gen_M)]:
        if f in feats and p(0.6): add(g(pkg, cs))
    d = os.path.join(root, pkg, "src", pkg)
    os.makedirs(d, exist_ok=True)
    for n, src in files.items():
        head = ch("", "// leading comment\n", "/* c */ ", "\n\n", "/** doc */\n")
        imports = ch("", "import java.util.*;\n", "import static java.lang.Math.*;\nimport java.io.Serializable;\n")
        with open(os.path.join(d, n + ".java"), "w") as fh: fh.write("%spackage %s;\n%s%s" % (head, pkg, imports, src))
    with open(os.path.join(root, pkg, "Probe.scala"), "w") as fh:
        fh.write("package %s\n%s  def main(args: Array[String]): Unit = {\n" % (pkg, PRELUDE))
        defs = [l for l in probe if l.startswith(("def ", "class ", "@An"))]
        fh.write("".join("    %s\n" % l for l in probe if l not in defs))
        fh.write("  }\n" + "".join("  %s\n" % l for l in defs) + "}\n")

if __name__ == "__main__":
    root, n, seed = sys.argv[1], int(sys.argv[2]), int(sys.argv[3])
    feats = sys.argv[4].split(",") if len(sys.argv) > 4 else ["K", "E", "R", "S", "An", "G", "I", "B", "M"]
    R.seed(seed)
    for i in range(n): gen_case(root, seed * 1000 + i, feats)
