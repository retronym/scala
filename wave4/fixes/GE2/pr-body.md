A super call with type arguments, from a class that fixes a specialized type parameter of a trait, dispatches back to the overriding method and overflows the stack:

```scala
trait T0[@specialized(Int) B] { def m[X](x: X, b: B): Int = 1 }
class C extends T0[Int] { override def m[X](x: X, b: Int): Int = super.m[X](x, b) + 1 }
```

`new C().m(1, 2)` throws `StackOverflowError` instead of returning 2. The type arguments may be inferred (`super.m(x, b)`), the method type parameter may itself be specialized, and the call may be curried, as in a `foldAll[Z](z: Z)(f: (Z, A) => Z)` of a specialized trait `Fold[@specialized(Int) A]` overridden in `IntFold extends Fold[Int]`. With a class parent the same program prints 2.

Specialization moves the body of `C.m` into `C.m$mcI$sp` and changes `C`'s parent to `T0$mcI$sp`. A call with type arguments is `Apply(TypeApply(Select(Super(...), m), targs), args)`, which goes to the `TypeApply` case of `SpecializationTransformer.transform`. That case finds the variant `m$mcI$sp` in the type of the `Super` tree as typer left it, the generic `T0[Int]`, and selects it by symbol: `T0.m$mcI$sp`, the forwarder in the generic trait that calls `m` virtually, which lands in `C.m` and then `C.m$mcI$sp` again. With a class parent the JVM resolves the `invokespecial` from the direct superclass, the specialized class, which hides the wrong symbol.

When the qualifier is a `Super` whose class has new parents, the `TypeApply` case now retypes the `Super` against the new parents and selects, by symbol, the member there that matches the variant: `T0$mcI$sp.m$mcI$sp`, the specialized trait's implementation. Selecting by symbol rather than by name matters when `m` is overloaded, since all the overloads' variants share the name `m$mcI$sp`.

This also changes what a super call with type arguments from another method runs. In `class S extends T0[Int] { override def m[X](x: X, b: Int) = 10; def n[X](x: X) = super.m[X](x, 3) }`, `n` used to reach `S.m` through the generic forwarder and return 10; it now runs `T0`'s `m` and returns 1, which is what `super` means.

The same lookup, `specSym`, crashed the compiler with an assertion for overloads of a method with type parameters that differ only in an unspecialized parameter type, `def m[X](x: X, b: B, c: Int)` and `def m[X](x: X, b: B, c: String)`, on any call, not only a super call: the AnyRef case of `doesConform` accepts both variants. When more than one variant passes, `specSym` now keeps the specialized variant of the called method, or an override of it. Only programs that crashed reach that branch.

Not a regression: 2.12.20 and 2.13.18 loop the same way. The same family is addressed for calls without type arguments in retronym/scala#354, which handles `super.m(args)` but not `super.m[X](args)`; its test covers a specialized method type parameter only with a class parent. This change is independent of retronym/scala#354 and composes with it. retronym/scala#433 adds an override lookup in `transformSelect` for a related shape.

Found in the fourth wave of the coordinated code review of 2.13.x (finding GE2). Prepared with Claude.
