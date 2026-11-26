A conditional, `match`, `try` or block whose branches are values of one value class crashes the compiler in erasure when it is used directly as the receiver of a field accessor, `==`, `!=` or a method inherited from a universal trait, and the branches' erased underlying types differ:

```scala
class Meters(val value: Double) extends AnyVal
def p(ok: Boolean, d: Double) = (if (ok) new Meters(d) else new Meters(0)).value

class G[T](val x: T) extends AnyVal
def q(c: Boolean, s: String) = (if (c) new G(s) else new G[CharSequence](s)).x
def r(c: Boolean) = (if (c) G(1) else G("s")).x
```

```
mergePrefixAndArgs(List(ErasedValueType(class Meters, Double), ErasedValueType(class Meters, Double(0.0))), covariant, Depth(1)): unsupported tps
```

The underlying types differ when one branch constructs the value from a constant (`new Meters(0)`, a `final val`, two different literals), from an argument narrower than the class's underlying type (`new R(s)` with `s: String` for `class R(val x: AnyRef)`, `new St(null)`, `new G(throw e)`), or when the branches are different instantiations of a generic value class, constructed or not (`G[String]` and `G[CharSequence]` values, `G(1)` and `G("s")`, a covariant `Wrap[+A]` built as `new Wrap(s)` and `new Wrap[AnyRef](s)`). Scala 3 compiles all of these; 2.10.7 compiles the constant case.

Erasure types a value of value class `C` as `ErasedValueType(C, U)`, where `U` is the erased type of the unboxed value: the erasure of the class's underlying type for a plain class, the boxed erasure of the type argument for a generic one, and the argument's own type for an injection `new C(e)` (the `InjectDerivedValue` case of `Erasure.Eraser.typed1`). When erasure retypes a conditional that is the qualifier of a selection it keeps as a member selection, `typedIf`, `typedMatch` (for the switches the pattern matcher emits) and `typedTry` take the lub of the branch types, and lub had no case for `ErasedValueType`: two different ones fell through to `mergePrefixAndArgs`, which aborts. Binding the conditional to a val, passing it as an argument, or calling a method that extmethods rewrites to an extension call avoids the lub and so the crash.

The fix gives `lub` a case for `ErasedValueType`s of one class: the result is `ErasedValueType(C, U)` with `U` the widest of the underlying types if one is wider than all the others, and `Object` otherwise. That `U` is the representation posterasure uses for the joined value: for a plain class every value's underlying type conforms to the erasure of the class's underlying type, so the widest one is a type of that representation (`Double` for `Meters`, `Object` for `R`), and for a generic class every instantiation's underlying type is a reference, so `Object` holds `Integer` and `String` alike; `G(1)` and `G("s")` joined keep the boxed `Integer`. A member selected from the joined value is cast to the member's type as for any other erased value, so a wider `U` costs at most a `checkcast`. Underlying types with no widest one that include a primitive cannot arise and keep the old behaviour. Only programs that crashed before reach the new case, so the code generated for code that compiled before is unchanged.

The change also types an injection whose argument has type `Nothing` (`new St(???)`, `new In(throw e)`, `new St(return 4)`) with the class's erased underlying type. Posterasure replaces `new St(e).x` by `e`, which kept type `Nothing`, so `new St(???).x.length` failed verification (`Nothing$` is not a `String`) and `new In(throw e).x + 1` crashed the backend (`Nothing$ is not a numeric or boolean type`). Scala 3.7.4 has the same two failures.

The crash for constant arguments is a regression from 2.10, in 2.11.0: scala/scala#3082 (SI-6385) made `ErasedValueType` carry the underlying type and typed the injection with the argument's type. The crash for different instantiations has the same origin. retronym/scala#427 teaches `-Ycheck` about the same injection trees; it accepts an injection whose argument type conforms to the underlying type, which holds for the `Nothing` case, and composes with this change.

Found in the fourth wave of the coordinated code review of 2.13.x (finding WP7). Prepared with Claude.
