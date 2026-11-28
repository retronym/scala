An argument of a Java annotation or a Scala `ConstantAnnotation` must be a constant expression (SLS 11, which refers to SLS 6.24: literals, constant values, `classOf`, enum constants, arrays, nested annotations, and operators on primitive and string constants). Since literal types, scalac accepts any expression whose type is a literal type instead:

```scala
object K { def three(): 3 = { println("three"); 3 } }
@Ann(z = { println("side effect"); false }, i = { val u = 1 / 0; 3 }) class A
@Ann(i = K.three()) class B
```

compile and store `false`, `3` and `3` without anything being evaluated. 2.12 rejects them with "annotation argument needs to be a constant", as does Scala 3 for a Java annotation. The stored value is the one the type guarantees, so nothing wrong is computed; what is lost is the error, and with it a reader's expectation that the side effects happen.

`tryConst` in `Typers.typedAnnotation` takes the value from the type of the typed argument: either a `Literal`, which the typer produces from any tree of foldable constant type (keeping the original in an `OriginalTreeAttachment`), or a tree of `ConstantType`. With literal types a block ending in a literal, a call of a method with a literal result type, and `id(5)` for `def id[T <: Singleton](t: T): T` all have such a type.

`tryConst` now also requires the original tree to be a constant expression: literals, references to stable values (whose qualifier is not evaluated, so `K.make().CONST` stays accepted for a Java constant field, as `jvm/java-constant-impure-qualifier` expects), and unary and binary operators on primitive values and strings applied to constant expressions, including a literal receiver as in `!true`, `~1` or `-(1)`. Parentheses, type ascriptions, named arguments and default arguments are accepted as before. Blocks and other calls are reported with the existing error, now showing the original expression. Arguments of annotations in Java sources are unchanged. The `pos` test lists the forms of constant expression that stay accepted, the `neg` test the forms now rejected.

Whether to reject at all is a decision for the owner. This change rejects source that 2.13.0 to 2.13.18 accept: a block, a call of a method with a literal result type, or an operator applied to such a call, as an annotation argument. The case for an error is that the specification is explicit, 2.12 and Scala 3 (for Java annotations) reject the same arguments, and the forms are unlikely in real code. The alternative is a warning that keeps compiling these programs with the value the type guarantees, turning into an error in a later release. The check also applies to Scala `ConstantAnnotation`s such as `@deprecated`, `@nowarn` and `@SerialVersionUID`, which Scala 3 does not check.

Regression in 2.13.0 from literal types (SIP-23), which gave these expressions a constant type that `tryConst` has accepted in place of a literal since long before. retronym/scala#606 changes when a block in ordinary code is folded to its literal and does not touch this check. No ticket found.

Found in the fourth wave of the coordinated code review of 2.13.x (finding GF2). Prepared with Claude.
