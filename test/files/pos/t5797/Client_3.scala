object Client {
  // "X is already defined as (compiler-generated) case class companion object X"
  // This happens because case class synthesis is not
  // idempotent and doesn't work after resetLocalAttrs.
  // OK.m    { case class X(); println(X().productPrefix) }

  // But here we're okay.
  KO.m(0) { case class X(); println(X().productPrefix) }

  //{ def foo = { case class XXX() } }

  { KO.m(1) { case class X(); println(X().productPrefix) } }
  KO.m(2) { case class X(); println(X().productPrefix) }
  KO.m(3) { case class X(); println(X().productPrefix) }
  KO.m(4) { case class X(); println(X().productPrefix) }
}
