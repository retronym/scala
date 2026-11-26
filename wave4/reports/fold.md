# Wave four spike: fold (primitive expression semantics)

## What was built

`wave4/harness/fold/fold.py` generates well-typed expression trees over the primitive types and String and prints each tree in up to 22 spellings that must agree at run time: literals inline (the typer folds), literals behind a `var`, a generic `@noinline` identity, local vals, `final val` and literal-typed vals in the same and in a separately compiled unit, conversions as `asInstanceOf`, operands boxed as `Any` and as `java.lang` boxes, leaves through default arguments, method parameters, tuples, captured vars, value classes, arrays and Option/Some/List, arithmetic through Numeric/Integral/Fractional/Ordering, compound assignment, and the whole expression as a default argument and as a Java annotation argument. Each batch is compiled with no optimizer flags and with `-opt:inline:** -opt:local`, so each tree yields up to 44 values. A form the compiler rejects is dropped and reported; disagreements are grouped by the partition of forms and shrunk by hoisting subtrees and substituting leaves. Scala 3 was not used as a form; it was run on the two reproductions only.

## Numbers

All on the integration build.

| round | trees | forms | result |
|---|---|---|---|
| 1, depth 4 | 300 | 10 | no rejection, no disagreement |
| 1, depth 5 | 1500 | 10 | 17 rejected forms, 3 disagreements in 2 signatures, all the harness's fault |
| 2, evolved (Unit/Null operands, blocks, throwing branch, type and literal matches on Any, 7 more forms) | 600 generated, 450 ran | 17 | one batch lost to a compiler crash (GF1); 1 disagreement, harness's fault |
| 3, breadth (try, while, literal syntaxes, arrays, Option, method parameters, type classes, compound assignment) | 960 generated, 240 ran | 22 | six batches lost to an invalid literal in the harness's pool; 10 disagreements in 5 signatures: 9 the harness's fault, 1 is GF2 |
| 4, after fixes | 150 + 480 | 22 | 2 disagreements, both the harness's fault |

False alarms and their causes: `.##:` lexed as one operator (17 rejections); an ascription to a `java.lang` box or a default-argument type gives an `if` an expected type, which turns off the weak-lub typing of its branches (4 disagreements, one still open for `try`); `Option(null).get` (10). No disagreement was found between the folder, the back end, the boxed forms and the optimizer on arithmetic, shifts, conversions, comparisons, equality, hashing or concatenation.

Rate: about 370 trees a minute with ten forms at depth 5, about 150 a minute with all forms, compiling through `bin/scalac`.

## Coverage

JaCoCo on the compiler JVM for one round-four batch (150 trees, 22 forms, both optimizer settings), line and branch share reached:

| class (with nested) | lines | branches |
|---|---|---|
| ConstantFolder (includes JavaConstantFolder) | 77% | 58% |
| ScalaPrimitives | 94% | 60% |
| BCodeBodyBuilder | 77% | 59% |
| BCodeIdiomatic | 81% | 72% |
| Erasure | 68% | 48% |
| TypeAdaptingTransformer | 71% | 56% |
| CleanUp | 43% | 46% |
| BoxUnbox (optimizer) | 83% | 36% |

Coverage was measured once, after the last generator change, so there is no per-round series. The largest unreached methods are `CleanUp.transformApplyDynamic` and its helpers (structural calls on primitives), the spill path of `genStringConcat` (very long concatenations), `JavaConstantFolder` (constants in Java sources), `genReturn` and `emitZeroOf`. No round was aimed at them.

## Findings

- GF1, crash: `(if (false) -1 else ???) match { case 3 => ...; case _ => ... }` fails in the back end with "Bad superClass for class Int". Present on the integration build, the base and 2.13.18; Scala 3.7.4 compiles it. No earlier finding.
- GF2, missing error: a block ending in a literal is accepted as a Java annotation argument and its statements are dropped. Present on all three Scala 2 builds; Scala 3.7.4 rejects it. Same family as EO6 and LZ2 in ordinary code.

Not recorded as findings: `1.toByte` and `1.toShort` are not folded (`foldableUnaryOps` omits them), so a Byte or Short member of a Java annotation takes only an Int literal while `65.toChar` is accepted for a Char member; this looks like a long-standing design gap and is the same on all three builds. An `if` with branches of different numeric types keeps each branch's own type when the expected type is `Any` (`"" + (if (c) 'a' else 1L)`) and widens otherwise; this is the documented typing rule.

## What the generator does not yet produce

Statements with side effects whose order could be observed; structural calls; Java-defined constants and mixed compilation; string interpolators; long concatenations; `return`; `lazy val`; specialization; pattern guards and extractors; literal patterns of Float and Double; `Unit`-typed arrays; trees deeper than five levels; Scala 3 as a form. A compiler crash still loses its whole batch, and the harness starts a compiler JVM per batch rather than reusing one.

## Is a longer campaign worth it

For the core target, probably not at this shape: about 3,100 trees and up to 44 values each found no wrong value in primitive arithmetic, conversion, equality, hashing or concatenation on a build that already carries the wave-two fixes. Both findings came from the second round's additions (Nothing-typed branches, annotation arguments), in the first batches that contained them. A further campaign should spend its time on new constructs and new embedding contexts, guided by the unreached methods above, rather than on more trees of the existing kind.
