A super call with type arguments, from a class that fixes a specialized type parameter of a trait, dispatches back to the overriding method and overflows the stack:

```scala
trait T0[@specialized(Int) B] { def m[X](x: X, b: B): Int = 1 }
class C extends T0[Int] { override def m[X](x: X, b: Int): Int = super.m[X](x, b) + 1 }
```

`new C().m(1, 2)` throws `StackOverflowError` instead of returning 2. The type arguments may be inferred (`super.m(x, b)`), the method type parameter may itself be specialized, and the call may be curried, as in a `foldAll[Z](z: Z)(f: (Z, A) => Z)` of a specialized trait `Fold[@specialized(Int) A]` overridden in `IntFold extends Fold[Int]`. With a class parent the same program prints 2.

Specialization moves the body of `C.m` into `C.m$mcI$sp` and changes `C`'s parent to `T0$mcI$sp`. A call with type arguments is `Apply(TypeApply(Select(Super(...), m), targs), args)`, which goes to the `TypeApply` case of `SpecializationTransformer.transform`. That case finds the variant `m$mcI$sp` in the type of the `Super` tree as typer left it, the generic `T0[Int]`, and selects it by symbol: `T0.m$mcI$sp`, the forwarder in the generic trait that calls `m` virtually, which lands in `C.m` and then `C.m$mcI$sp` again. With a class parent the JVM resolves the `invokespecial` from the direct superclass, the specialized class, which hides the wrong symbol.

When the qualifier is a `Super` whose class has new parents, the `TypeApply` case now selects the variant by name on a fresh `Super` and retypes it, so the call binds to `T0$mcI$sp.m$mcI$sp`, the specialized trait's implementation. This is what the case for `super.m(args)` without type arguments does, retyping against the new parents.

Not a regression: 2.12.20 and 2.13.18 loop the same way. The same family is addressed for calls without type arguments in retronym/scala#354, which handles `super.m(args)` but not `super.m[X](args)`; its test covers a specialized method type parameter only with a class parent. This branch is independent of retronym/scala#354 and merges with it without conflict; with both, every row of both tests passes. retronym/scala#433 adds an override lookup in `transformSelect` for a related shape.

Found in the fourth wave of the coordinated code review of 2.13.x (finding GE2). Prepared with Claude.
