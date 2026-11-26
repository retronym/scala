# Spike: erasure track (program generator, metamorphic oracles)

## What was built

A seeded generator of small typed programs and a single-JVM driver that compares each program's output with five rewrites (drop `@specialized`, drop `extends AnyVal`, separate compilation, `-Ydelambdafy:inline`, `-opt:inline:**`), plus `-Ycheck:all` on a sample and a dump mode for comparing two compilers. Sources and details: `wave4/harness/erasure/`. After the steering change the grammar was widened to pattern matching, for-comprehensions, try/finally, loops, local recursion, partial functions, interpolation, named arguments, implicits, nested objects and an evaluation-order trace, and the weight of `@specialized` was lowered from 0.6 to 0.25 per type parameter.

## Numbers

- Round 1 (first grammar): the first run compiled 1 of 178 programs because of two generator faults (indexing a fresh array passed the index as the ClassTag; applying a block). After the fix a 200 s run on two JVMs overran because shrinking was not bounded by the deadline; it was stopped by hand.
- Round 2 (first grammar): 21 and about 20 programs in 190 s. The reference threw outside the per-call try in about half of them (array index 1 on a one-element array in a constructor argument); fixed in the generator.
- Round 3 (widened grammar, 170 s each): with `@specialized` off, 17 programs, 17 compiled, no difference in `nospec`, `novc`, `separate`, `delambdafy-inline` or `-Ycheck:all`; two `-opt:inline:**` compilations exceeded the 60 s timeout, and the inliner variant took 130 of the 170 s. With `@specialized` at 0.25, 40 programs, 28 compiled as generated and the other 12 compiled once `@specialized` was removed (the compiler rejections below).
- Dump mode, integration build against 2.13.18, `@specialized` at 0.25: 254 programs in 75 s; 2 failed for a generator reason (ambiguous overload), 3 crashed the compiler. Of 210 programs compared, 10 differed: StackOverflowError or VerifyError on 2.13.18 that the integration build no longer shows, and none in the other direction.
- Programs per minute per JVM: about 14 with all rewrites, about 6 when the inliner variant dominates, about 200 in dump mode.
- False alarms: the driver reported two programs that fail to compile in both the reference and the variant as differences (line numbers differ under separate compilation); fixed in the signature function after the last run.
- Coverage of the round with `@specialized` off (JaCoCo, lines): `transform` 69%, `transform.patmat` 68%, `backend.jvm` 69%, `backend.jvm.opt` 80%, `backend.jvm.analysis` 79%, `typechecker` 47%, `transform.async` 2%. Largest partly reached classes by missed lines: `CleanUp.CleanUpTransformer` (232), `SpecializeTypes` (221), `BCodeBodyBuilder.PlainBodyBuilder` (192), `UnCurry.UnCurryTransformer` (171), `MatchOptimization.SwitchMaker` (82). These point at structural calls, symbol and array literals, Java varargs, and switches over three or more Int or String literals as the next grammar features. Only this one round was measured.

## Findings

Each is recorded in `wave4/findings/GE<n>/`. All were run on the integration build, the base and 2.13.18 with `three.sh` in `wave4/harness/erasure/`. Scala 3 was not run. None matches an entry in the earlier findings lists unless stated.

GE1 (crash): a trait overrides a method that has a `@specialized` method type parameter and calls `super.m[B](p)`. The compiler crashes in `SpecializeTypes.addBody` with `NoSuchElementException: key not found: method super$m`. Same on all three builds; without `@specialized` the program prints 2. No earlier id found.
GE2 (miscompile): a class extends a `@specialized` trait at Int and overrides a method that has its own type parameter, calling `super.m[X](x, b)`. Expected 2; actual StackOverflowError on all three builds. The same shape without the method type parameter is wave-one B2, which is fixed on the integration build and still fails on the base, so this is an instance of B2/B13 that the fix does not cover.
GE3 (wrong rejection): a class fixes a `@specialized` parameter at Long and overrides a method with a by-name parameter of that type. All three builds reject it with "double definition ... have same type after erasure" between `q: => Long` and `q: () => Long`. Without `@specialized` it compiles. No earlier id found.
GE4 (wrong rejection): a partial function literal with a guard inside a method with a `@specialized` type parameter. All three builds reject it with "type mismatch; found: x1.type (with underlying type Int), required: A1". This was 12 of the 40 programs in round 3. Nearest earlier entry is H66 (a type test rejected in a specialized body); the construct differs.
GE5 (wrong rejection): a local lazy val of the specialized type captured by a lambda inside a local def, in a method with a `@specialized` type parameter. All three builds reject it with "type mismatch; found: Int, required: x.type". A lambda that captures the lazy val directly, without the local def, compiles. No earlier id found.
GE6 (regression; 39 KB repro, not shrunk): `-opt:inline:** -opt:local` crashes the integration build and the base with `scala.tools.asm.LimitExceededException: Too many allocated bytes` from `ProdConsAnalyzer` under `ClosureOptimizer`; 2.13.18 compiles the file in 2 min 19 s. Candidate regression on 2.13.x since 2.13.18; no earlier id found. Not minimised because each compilation takes one to two minutes.
GE7 (crash; 19 KB repro): `FatalError: EmptyScope.enter` during constructors on all three builds. Not minimised; which feature triggers it is not established.
GE8 (crash; 32 KB repro): `AssertionError: sym: AC$mcBI$sp.m42$mcIB$sp target: AC.m42$mcI$sp` during specialize on all three builds. Not minimised.

GE1 and GE2 are switched off in the generator by default (`-Dw4e.superpoly=true` restores them) so that they do not mask others.

## What the generator does not yet produce

Scala 3 comparison; stackable traits with `abstract override`; overriding vals; secondary constructors; Java interop; structural types; string and Int switches; inner classes with outer references; implicit resolution beyond one implicit class and one implicit value; value classes wrapping arrays or functions. Shrinking works on whole lines only and does not shrink reference crashes.

## Does the target deserve a longer campaign

Yes for the specialization rewrite: every signature that reproduced came from it, at roughly one new root cause per 30 programs, and three are compiler rejections of valid code that a hand-written probe would be unlikely to combine. With `@specialized` off, 17 programs showed nothing, which is too few to judge the general oracles. Before a longer run: bound or drop `-opt:inline:**` (use a narrower inline scope or smaller methods), add in-line shrinking, and mask the known specialization rejections so that other differences surface.
