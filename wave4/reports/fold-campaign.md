# Wave four campaign: fold (embedding contexts)

## What was built

`wave4/harness/fold/ctx.py` takes the spike's expression trees (`fold.gen`, evolved grammar, depth up to 4), wraps about 40% of the leaves in a traced identity, and places each tree in the hole of one of about 170 context templates in five families: control flow (`ctl`), definitions and fields (`def`), control flow in expression position (`lift`), argument-order rewrites (`named`), and calls, strings and arrays (`dyn`). The README in the harness directory lists them. `cov.py` turns a JaCoCo exec into lines per scope file and the largest unreached methods.

Oracles:

- Spelling: the hole is filled inline, bound to a local val, through an `@noinline` method, and through an applied closure, each compiled with and without `-opt:inline:** -opt:local`; the eight value-and-trace lines must agree.
- Specification (from round 2): each context has a hand-written expected trace of its own markers with the expression's trace in place, checked whenever the expression alone does not throw.

Scala 3.7.4 was run on the reproduction only. The build under test was `w4-int` from round 1 on.

## Numbers

| round | change | cases | seconds | signatures | false alarms |
|---|---|---|---|---|---|
| 1 | 46 control-flow contexts, four spellings, optimizer off/on | 3600 | 558 | 0 | 0 |
| 2 | lift family (30 contexts) and the specified-trace oracle | 2100 | 430 | 0 | 0 |
| 3 | dyn family with def and named families | 2400 | 436 | 0 | 0 |

Smoke runs before each round (about 1,000 cases) also found no disagreement; their rejections were the templates' fault and were fixed: a refinement needs an `Any` parent for a primitive receiver, `Array(...)(i)` passes `i` as the ClassTag, and `({ ...; b })(i) += x` is rejected by Scala 2 ("receiver is not assignable") while Scala 3.7.4 accepts it, a documented restriction.

Lines covered (round 1 measured on a 900-case replay of the same seed, because the first JaCoCo run missed boot-classpath classes):

| file | round 1 | round 2 | round 3 |
|---|---|---|---|
| CleanUp | 131/346 | 137/346 | 278/346 |
| Fields | 113/276 | 203/276 | 260/276 |
| Constructors | 177/337 | 192/337 | 207/337 |
| UnCurry | 302/488 | 344/488 | 267/488 |
| Erasure | 568/823 | 574/823 | 581/823 |
| RefChecks | 608/1371 | 610/1371 | 648/1371 |
| BCodeBodyBuilder | 723/917 | 725/917 | 713/917 |
| BCodeSkelBuilder | 255/335 | 260/335 | 269/335 |
| ConstantFolder | 151/176 | 147/176 | 158/176 |
| ScalaPrimitives | 272/288 | 272/288 | 261/288 |

Round 3 did not run the lift family, so its UnCurry figure falls back. Treemap: `wave4/reports/fold-treemap.html`.

## Findings

- WX1, wrong result: a structural call to a `Unit` method through a refinement member whose result type is `Any` returns `null` instead of `()`. Present on the integration build, the base and 2.13.18; Scala 3.7.4 behaves the same. It came from a hand probe of `CleanUp.fixResult` while writing the round-3 contexts from the coverage gap, not from a generated case. Added to `known-signatures.md`.

Hand probes that found nothing: try/finally, return and while in super-constructor, auxiliary-constructor, early-definition and argument positions; structural calls with primitive-op names on `java.lang.Number` subclasses, arrays, generic, varargs and Unit members; Dynamic apply, named, update and chained selections.

## What the generator does not produce

`DelayedInit` (`App`), static initialisers, more than 200 partial strings in one concatenation, non-local return from a lambda inside a lifted try (`UnCurry.nonLocalReturnTry`), override checks (`RefChecks` remains half reached and is mostly diagnostics), Java-defined members and mixed compilation.

## Is more worth it

Not at this shape. About 8,100 generated cases in some 170 contexts, with a specified-trace oracle, found no wrong value or evaluation order on the integration build, which carries the wave-two and wave-three fixes for exactly these areas (lifted try, structural-call order, concatenation order). The one finding came from reading the code that coverage pointed at. Stop rule: rounds 2 and 3 gained coverage but no signature; the budget ended there.
