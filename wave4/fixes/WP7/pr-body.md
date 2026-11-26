For a value class `class Meters(val value: Double) extends AnyVal`, a conditional that constructs it in both branches, one from a constant, used directly as the receiver of a field accessor, `==`, `!=` or a method inherited from a universal trait, crashes the compiler in erasure:

```scala
def p(ok: Boolean, d: Double) = (if (ok) new Meters(d) else new Meters(0)).value
```

```
mergePrefixAndArgs(List(ErasedValueType(class Meters, Double), ErasedValueType(class Meters, Double(0.0))), covariant, Depth(1)): unsupported tps
```

The same happens with a `match` or a block ending in an `if`, with a `final val` constant argument, with two different constants, and without any constant when an argument's type is narrower than the class's underlying type (`new R(s)` and `new R(o)` for `class R(val x: AnyRef)`, `s: String`; `new St(null)` against a value of type `St`). Scala 3 compiles all of these; 2.10.7 compiles the first.

Erasure represents the injection `new VC(e)` by `e` typed as `ErasedValueType(VC, U)` (the `InjectDerivedValue` case of `Erasure.Eraser.typed1`). `U` was the type of the retyped argument, so `new Meters(0)` got `ErasedValueType(Meters, Double(0.0))` while `new Meters(d)` and every other value of type `Meters` got `ErasedValueType(Meters, Double)`. When erasure retypes a conditional that is the qualifier of a selection it keeps as a member selection, `typedIf` / `typedMatch` take the lub of the branch types, and lub cannot merge two `ErasedValueType`s that differ in their underlying type. Binding the conditional to a val, passing it as an argument, or calling a method that extmethods rewrites to an extension call (`equals`, `hashCode`, `toString`, user methods) avoids the retyping as a receiver and so avoids the crash.

The fix types the injection with the class's erased underlying type (`erasedValueClassArg`, already used as the expected type for the argument), so an injection has the same type as any other value of the class. The argument tree itself is unchanged; it only loses its constant or narrower type as the type of the injection. Posterasure's elimination of `new VC(e).underlying` and of `new VC(e1) == new VC(e2)` matches trees, not these types, and constant folding does not run after typer, so the code generated for ordinary uses is the same.

This is a regression from 2.10, in 2.11.0: scala/scala#3082 (SI-6385) made `ErasedValueType` carry the underlying type and has typed the injection with the argument's type since. retronym/scala#427 teaches `-Ycheck` about the same injection trees; it accepts an argument whose type conforms to the underlying type and composes with this change.

Found in the fourth wave of the coordinated code review of 2.13.x (finding WP7). Prepared with Claude.
