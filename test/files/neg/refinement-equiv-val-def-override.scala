trait Mem { type T; def get: T; def put(t: T): Unit }

trait Base { def m(x: AnyRef { def f: Mem }): Unit }
// not an override of Base.m: `x.f` is a path here, and is not one in Base
class Derived extends Base { def m(x: AnyRef { val f: Mem }): Unit = { val t: x.f.T = x.f.get; x.f.put(t) } }
