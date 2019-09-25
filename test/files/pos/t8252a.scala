trait Foo { type X }
trait HL extends Foo { override type X }
trait HC[H <: Foo, T <: HL] extends HL { override type X = H#X with T#X }
trait HN extends HL { override type X = Any }
class A extends Foo { trait X } ; class B extends Foo { trait X }
class Test {
    def test: Unit = {
        val bad = new HC[A, HC[B, HN]] {}
        val xx: bad.X = ???
    }
}
