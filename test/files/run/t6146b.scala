import scala.tools.partest.ReplTest

object Test extends ReplTest {
  override def code = """
  | :power
  | val u = rootMirror.universe
  | import u._, language._
  | class C { object O { class E }}; object D extends C
  | deepMemberType(typeOf[D.type], typeOf[c.O.E forSome { val c: C }].typeSymbol)
  | class C { class C1 { class E; object E }}; object D extends C { class D1 { object D2 } }
  | deepMemberType(typeOf[D.type], typeOf[c1.E forSome { val c1: C#C1 }].typeSymbol)
  | deepMemberType(typeOf[d1.D2.type forSome { val d1: D.D1 }], typeOf[c1.E.type forSome { val c1: C#C1 }].typeSymbol)
  """.stripMargin.trim
}