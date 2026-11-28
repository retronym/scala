# Campaign: programs2 track (general program generator, second campaign)

Continues the programs campaign (`programs-campaign.md`) on the same generator and shared driver, extended in place in `wave4/harness/programs/` (README there). Build under test: `w4-int` throughout. `@specialized` at weight 0.

## What changed in the harness

- Six construct families, one per round, chosen from the largest unreached code in `transform`, `transform/patmat` and `backend/jvm`: non-local return; pattern matching; local and nested classes and objects; trait linearization and lazy vals; value classes meeting earlier constructs, defaults, named arguments and varargs from sequences; `Dynamic`, interpolators and `for` comprehensions over several collections.
- Each program favours two of the families switched on (weight 4), so that constructs of different rounds meet in one program. Rounds 7 to 10 nest at depth `d - 2`; round 2 grew programs about 1.5x and dropped throughput to 5 and 20 programs a minute before that.
- Subexpression shrinking in the generator's syntax (hoist a nested group, drop a block statement or a case arm) after unit and line deletion, and a `shrinkfile` mode, including shrinking a crash of the reference alone. Every finding below came out of it at 200 to 600 characters of the interesting member.
- A 2.13.18 difference is re-run on the unfixed base and classified automatically (fixed upstream after 2.13.18, or fixed by an earlier wave); per-oracle difference counts; `-opt:local` only on sources up to 45,000 characters.
- Scala 3.7.4 used by hand on reproductions only.

## Rounds

| Round | Family | Programs | Compiled | New | Known | transform | patmat | backend/jvm |
|---|---|---|---|---|---|---|---|---|
| 1 | triage of the two open signatures; non-local return | 431 | 99.5% | 4 (WP4 to WP7) | 6 | 3989/5328 | 1566/2186 | 2802/3905 |
| 2 | pattern matching | 93 | 98.9% | 0 | 5 | 3999 | 1628 | 2731 |
| 3 | local and nested classes and objects | 314 | 99.7% | 1 (WP8) | 6 | 4019 | 1628 | 2733 |
| 4 | stackable traits, init order, lazy vals | 112 | 100% | 1 (WP9) | 2 | 4024 | 1730 | 2744 |
| 5 | value classes meeting the rest; defaults, named args, varargs | 124 | 99.2% | 0 | 4 | 4019 | 1628 | 2770 |
| 6 | Dynamic, interpolators, for comprehensions | 635 | 99.5% | 0 | 7 | 4061 | 1628 | 2738 |

1,709 programs, about 23,000 compilations. Coverage per round is from the one JaCoCo JVM of that round (90 to 120 programs), so it moves with the sample; the union over the six rounds is transform 4073 (programs campaign 3916), patmat 1730 (1566), backend/jvm 2841 (2792). Newly reached lines are mostly in `UnCurry` (73), `MatchAnalysis` (85) and `MatchTranslation` (43). Rounds 4 to 6 ran one JVM under JaCoCo and used the second slot for shrinking. Treemap: `reports/programs2-treemap.html` (the programs campaign's round 4 and these six rounds).

Oracle disagreements over all rounds (differences/programs): `-opt:local` 13/1,446 (5 in round 2 from GE6 timeouts, the rest WP5 and its -opt:local AnalyzerException), `-Ydelambdafy:inline` 0/1,709, separate compilation 0/1,709, 2.13.18 292/1,709 (285 fixed by earlier waves, 5 changed upstream after 2.13.18, 2 left for triage: one an earlier wave's fix, one a WP5 program), `-Ycheck:all` 138/176, inliner 0/4. Exclusions fired: v18-wavefix 285, WP6 102, WP1 15, WP9 15, WP5 8, v18-upstream 5, GE6 4, WP2 2, WP8 1.

Not findings: `@tailrec` with a `return` in the argument of the recursive call is rejected by 2.13.18 and accepted by the base and the integration build (changed upstream). One 2.13.18 difference where the base matched neither side was the integration build running a finalizer that the base and 2.13.18 skip, i.e. an earlier wave's fix. Generator faults, fixed after their rounds: anonymous classes in value classes through default arguments, a lambda in a secondary constructor's self-invocation (an implementation restriction), a for-comprehension filter on a later generator's variable, `foldLeft` and `Bn.twice` type arguments inferred narrower or as `AnyVal` around a `return`.

## Findings

- WP4 (spurious -Ycheck): a by-name argument that reads a trait's lazy val of a type-parameter type, in a subclass; the thunk's result type is the singleton `C.this.lz.type`, wrong once fields makes the lazy val a method. The untriaged signature from the programs campaign's round 4.
- WP5 (crash): a `return` evaluated with operands on the stack (in a method argument) in a try/finally with another return; `genReturn` jumps to the cleanup without emptying the stack, ASM's frame computation throws ArrayIndexOutOfBoundsException. 2.13.18 and Scala 3.7.4 emit code that fails verification. CF2's cause (retronym/scala#618), new trigger.
- WP6 (spurious -Ycheck): a `return` inside a partial function literal is re-typed against `applyOrElse`'s result type from typer to patmat; the other half of retronym/scala#460. It fired on most `-Ycheck` samples once returns were generated.
- WP7 (crash): `(if (c) V0(b) else V0(7)) == V0(5)` with a value class `V0` crashes erasure: the injection's `ErasedValueType` keeps the literal's constant type and lub cannot merge it. All three 2.13 builds; Scala 3 correct. Came from the round-2 switch family meeting value classes.
- WP8 (spurious -Ycheck, needs a second look): the non-local-return handler's `ex.value()` in a method with a type parameter.
- WP9 (spurious -Ycheck): a constructor pattern against `List[T]()`, which the typer rewrites to `Nil`.

The other open signature from the programs campaign's round 4 (`box(RichI.dbl$extension(...))`) is WP1: a guarded string switch with a boxed default arm.

## Not yet produced

Java-facing members (generic signatures, static forwarders, `appendToStaticCtor`), symbol and array literals (`CleanUp.transformApply`), string concatenation of many parts, `abstract override` combined with generics, Java sources. The pattern family does not reach `MatchAnalysis` counterexamples (the harness compiles with `-nowarn`). `-Ycheck:all` now disagrees on three quarters of its samples, all from known checker reports; a WP6-free sample would make it informative again.

## Longer campaign?

The yield pattern held: the two crashes came in the first round that generated returns (WP5) and from constructs of two rounds meeting (WP7); later rounds added coverage and only checker reports. Rounds 5 and 6 found nothing new but gained coverage, so the stop rule was not reached. A further campaign should start from the unreached Java-facing and literal code above rather than add more expression forms.
