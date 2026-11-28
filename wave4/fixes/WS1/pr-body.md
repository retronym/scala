A lambda whose result type is an existential over local classes is rejected when UnCurry expands it into an anonymous class:

```scala
object M {
  def f = { trait A; abstract class K extends A; trait C extends K; (null: C) }
  def g = () => f
}
```

```
error: type mismatch;
 found   : scala.runtime.AbstractFunction0[C] with java.io.Serializable
 required: () => C forSome { type C <: AnyRef with A; type A <: AnyRef }
```

The program typechecks; the error comes from the typer UnCurry runs on the expansion. Lambdas are expanded under `-Ydelambdafy:inline`, which mdoc sets for every Scala 2 compile (scala/bug#9824) and Scastie for every 2.12 and 2.13 snippet (scala/bug#10782), and also under the default strategy for a lambda in a super-constructor argument or an early definition (`class Q extends P(() => M.f)`). `run/existentials3-new`, `run/existentials3-old` and `pos/existentials` fail the same way under `-Ydelambdafy:inline`. Scala 2.11 and Scala 3 accept the program.

Typer gives the lambda the type `() => C forSome { type C <: AnyRef with A; type A <: AnyRef }`, packing the body's type. `expandFunction` built the anonymous class's parent from the body's type instead, `AbstractFunction0[C']` with `C'` an existential skolem whose bound names another skolem `A'`, and then re-typed `new $anon(): () => C forSome {...}`. `AbstractFunction0[C'] <: () => C forSome {...}` fails because `A` occurs only in the bound of `C`, so the solver never learns `A := A'`. That weakness of existential subtyping with dependent quantifier bounds is old; the expansion should not have to re-check what typer packed.

This is a regression from 2.12.0. 2.11's UnCurry took the parent's type arguments from the function type (`abstractFunctionForFunctionType(fun.tpe)`). The SAM refactoring in 2.12.0 dropped the `AbstractFunctionN` parent, and the commit that brought it back ("Bring back AbstractFunction parent", also in 2.12.0, kept for the mixin forwarders it saves) rebuilt the type from the parameter symbols and `fun.body.tpe.deconst`. Nothing in that commit depends on using the body's type.

The fix takes the type arguments from `fun.tpe` again. The parameter types are the same as before (typer builds `fun.tpe` from the parameter symbols), and the lifted `apply` method already took its result type from `fun.tpe`, so the parent and the `apply` it is implemented by now agree. Two visible differences in expanded classes:

- A lambda or by-name thunk with a constant body (`() => 42`, `() => true`, `(x: Int) => 1.0`) now extends the specialized `AbstractFunctionN$mc..$sp` for its arity and primitive types, as in 2.11. The body's type `42` survives `deconst` since literal types, so the parent was `AbstractFunction0[42]`, which specialization does not match. `run/delambdafy_uncurry_byname_inline.check` changes from `AbstractFunction0[1]` to `AbstractFunction0[Int]` accordingly.
- A lambda returning a class local to its body (`() => { class L; new L }`) has the generic superclass `AbstractFunction0<Object>` instead of naming the local class, which is defined inside the `apply` method.

Other expanded lambdas, including primitive ones that extend `AbstractFunction1$mcII$sp` and the like, compile to the same class files.

No scala/bug ticket covers this. scala/bug#10564 is a different `-Ydelambdafy:inline`-only failure.

Found in the fourth wave of the coordinated code review of 2.13.x (finding WS1). Prepared with Claude.
