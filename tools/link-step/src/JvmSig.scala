package linkstep

/** A parser for JVM generic signatures (JVMS 4.7.9.1) and plain descriptors, into a small AST. */
object JvmSig {
  sealed trait Sig
  final case class Prim(tag: Char) extends Sig
  final case class TVar(name: String) extends Sig
  final case class Arr(elem: Sig) extends Sig
  /** `Lpkg/Outer<..>.Inner<..>;`: the binary name joins the segments with `$`, the arguments are the last segment's. */
  final case class Cls(binaryName: String, args: List[Arg]) extends Sig

  sealed trait Arg
  case object Wild extends Arg
  final case class Exact(sig: Sig) extends Arg
  final case class Plus(sig: Sig) extends Arg
  final case class Minus(sig: Sig) extends Arg

  final case class TParam(name: String, bounds: List[Sig])
  final case class ClassSig(tparams: List[TParam], superclass: Cls, interfaces: List[Cls])
  final case class MethodSig(tparams: List[TParam], params: List[Sig], result: Sig)

  def classSig(s: String): ClassSig = { val p = new P(s); val tps = p.tparams(); val sup = p.cls(); val is = p.many(p.peek == 'L')(p.cls()); ClassSig(tps, sup, is) }
  def methodSig(s: String): MethodSig = {
    val p = new P(s)
    val tps = p.tparams()
    p.expect('(')
    val ps = p.many(p.peek != ')')(p.sig())
    p.expect(')')
    MethodSig(tps, ps, p.sig())
  }
  def fieldSig(s: String): Sig = new P(s).sig()

  private final class P(s: String) {
    var i = 0
    def peek: Char = if (i < s.length) s.charAt(i) else '\u0000'
    def next(): Char = { val c = s.charAt(i); i += 1; c }
    def expect(c: Char): Unit = { val n = next(); if (n != c) throw new IllegalArgumentException(s"expected $c at ${i - 1} in $s") }
    def many[A](cond: => Boolean)(a: => A): List[A] = { val b = List.newBuilder[A]; while (cond) b += a; b.result() }
    def ident(stops: String): String = { val st = i; while (i < s.length && stops.indexOf(s.charAt(i)) < 0) i += 1; s.substring(st, i) }

    def tparams(): List[TParam] =
      if (peek != '<') Nil
      else {
        next()
        val r = many(peek != '>') {
          val n = ident(":")
          // class bound (may be empty) then interface bounds
          val bs = many(peek == ':') { next(); if (peek == ':' || peek == '>') None else Some(sig()) }.flatten
          TParam(n, bs)
        }
        expect('>')
        r
      }

    def sig(): Sig = next() match {
      case 'L' => i -= 1; cls()
      case 'T' => val n = ident(";"); expect(';'); TVar(n)
      case '[' => Arr(sig())
      case c   => Prim(c)
    }

    def cls(): Cls = {
      expect('L')
      var name = ident("<.;")
      var args = typeArgs()
      while (peek == '.') { next(); name = name + "$" + ident("<.;"); args = typeArgs() }
      expect(';')
      Cls(name, args)
    }

    def typeArgs(): List[Arg] =
      if (peek != '<') Nil
      else {
        next()
        val r = many(peek != '>')(peek match {
          case '*' => next(); Wild
          case '+' => next(); Plus(sig())
          case '-' => next(); Minus(sig())
          case _   => Exact(sig())
        })
        expect('>')
        r
      }
  }
}
