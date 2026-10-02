package tastytest

object TestOpaqueConstructorParameter extends Suite("TestOpaqueConstructorParameter") {
  import OpaqueConstructorParameter._

  test("constructor with opaque type parameters") {
    val holder = new OpaqueHolder(id(42L), id("hello"))

    assert(holder.longId == 42L)
    assert(holder.stringId == "hello")
  }

  test("method signatures with polymorphic opaque types") {
    assert(Api.unbounded(id2(1)) == 1)
    assert(Api.array(arr(Array(1))) == 2)
    assert(Api.nested(id(id(1L))) == 3)
    assert(Api.arrayOfId(null: Array[Id[Int]]) == 4)
    assert(Api.arrayOfId2(null: Array[Id2[Int]]) == 5)
    assert(Api.result == 6)
  }

  test("implement Scala 3 trait with applied higher-kinded abstract type") {
    val a: AbstractHK = new AbstractHK { type F[X] = X; def f(x: Int) = 7 }
    assert(a.f(1.asInstanceOf[a.F[Int]]) == 7)
  }

  test("override Scala 3 method taking polymorphic opaque type") {
    val o: Overridable = new Overridable { def m(x: Id[Long]) = 8 }
    assert(o.m(id(1L)) == 8)
  }
}
