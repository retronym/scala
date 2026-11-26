# Campaign: programs3 track (general program generator, third campaign)

Continues the programs and programs2 campaigns on the same generator and shared driver, extended in place in `wave4/harness/programs/` (README there). Build under test: `w4-int` throughout. `@specialized` at weight 0. Rounds are numbered 11 to 15 after programs2's ten.

## What changed in the harness

- Five construct families, one per round (`-Dw4p.round=11..15`): members seen from Java; arrays of every element type and symbols; trait mixin, early initialisers, `App` and `DelayedInit`; outer references three deep, outer tests and lambdas in constructors; bridges, covariant overrides, `Enumeration`, implicit value classes and long string concatenation. Each program still favours two of the switched-on families.
- A reflection oracle inside the generated program: `Refl.desc` prints every generated class's generic signatures, bridge, default and varargs flags and fields; `Refl.getters` and `Refl.statics` invoke bean accessors and static forwarders; `Refl.ser` round-trips objects and lambdas through serialization. Its output is compared across all configurations like the rest of the program's output.
- `-Ycheck:all` on 1 seed in 20. New exclusions: `dl-ser`, WP10, and a 2.13.18 crash with WP5's message.
- `union.py`: union line coverage across rounds and campaigns.

## Rounds

| Round | Family | Programs | Compiled | New | Known | Union transform | Union patmat | Union backend/jvm |
|---|---|---|---|---|---|---|---|---|
| 11 | Java-facing members, reflection, serialization | 449 | 99.1% | 1 (WP10) | 8 | 4076 -> 4089 | 1730 | 2841 -> 2846 |
| 12 | arrays of every element type, symbols | 239 | 100% | 0 | 6 | 4108 | 1730 | 2854 |
| 13 | mixin, early initialisers, App, DelayedInit | 200 | 97.0% | 0 | 5 | 4174 | 1754 | 2861 |
| 14 | outer references, outer tests, lambdas in constructors | 295 | 99.7% | 0 | 5 | 4175 | 1765 | 2866 |
| 15 | bridges, Enumeration, implicit value classes, long concatenation | 509 | 99.8% | 0 | 5 | 4177 | 1776 | 2867 |

1,692 programs, about 21,000 compilations, 230 s per round with two JVMs (one under JaCoCo at 12 to 19 programs a minute, one at 49 to 116). "Union" is the union of line coverage over the three campaigns' JaCoCo rounds, of 5328, 2186 and 3905 lines; the campaign added 101 lines in `transform` (Constructors 52, Erasure 19, ExplicitOuter 13, CleanUp 9, UnCurry 7), 46 in `patmat` (MatchTreeMaking 35, Logic 11) and 26 in `backend/jvm`. Per-round figures and largest unreached methods: `wave4/rounds/programs3.json`; treemap: `reports/programs3-treemap.html`. The stop rule was not reached (rounds 12 to 15 found nothing new but each gained coverage); the campaign stopped at five rounds.

Oracle differences over all rounds (differences/programs): `-opt:local` 4/1,111 (WP5), `-Ydelambdafy:inline` 30/1,691 (28 dl-ser, 2 from one driver OutOfMemoryError), separate compilation 1/1,691 (the same OOM), 2.13.18 534/1,691 (533 fixed by earlier waves, 1 WP5), `-Ycheck:all` 65/84, inliner 0/0 (it shares its 1-in-20 seeds with no program small enough). No run-time VerifyError, ClassFormatError, AbstractMethodError, IllegalAccessError, IncompatibleClassChangeError or ClassCastException appeared in any configuration that the reference did not also show.

Exclusions fired: v18-wavefix 533, WP6 30, dl-ser 28, WP9 27, WP5 5, WP1 4, WP8 4, WP10 1.

False alarms and generator faults: one OutOfMemoryError of the 2 GB driver with three programs in flight (4 signatures in round 11; the two seeds compile and agree when run alone; later rounds ran two in flight); dl-ser, a by-design difference of `-Ydelambdafy:inline`, where a closure nested in a closure holds its enclosing closure and serialization then fails or differs; `@varargs` on a method with a ClassTag context bound (correctly rejected); `Array[Char]('a')(0)` passing 0 as the ClassTag; returns in trait members of local objects and in early initialisers; one return-type fault in rounds 14 and 15 not traced.

Not findings: a `return` in an early initialiser is rejected with "value <local q> has return statement; needs result type" and in an anonymous class's val with "return outside method definition"; neither initialiser is a method body, so both are rejections by the specification, with an odd message for the first.

## Findings

- WP10 (wrong rejection): `if (Debug) -1 else g(xs: _*)` with a constant `Debug` fails with "Sequence argument type annotation `: _*` cannot be used here". RefChecks reduces the constant `if` and transforms only the surviving branch's children, so the branch's own `Application` case never sets `currentApplication`. Present on the integration build, the base and 2.13.18; Scala 3.7.4 compiles it. The current revision of retronym/scala#750 (GF1) makes the change that fixes it; `w4-int` carries an earlier revision. #750 has no test with `: _*`.

## Not yet produced

Java sources calling the Java-facing members (the driver compiles Scala only); a static field initialiser (`appendToStaticCtor`); `MatchAnalysis` counterexamples (the harness compiles with `-nowarn`); `CleanUp.transformApplyDynamic` beyond the structural calls of programs round 3; `genStringConcat`'s chunking lambda, still unreached with 200 to 280 parts.

## Longer campaign?

Not on this generator. Over 3,000 earlier programs and these 1,692, new constructs kept adding coverage but the last eight rounds found one defect between them, and that one came from two earlier families meeting (value-class varargs forwarding under a generated constant condition) rather than from the round's own family. A further campaign should add a Java-source oracle for the Java-facing members or turn warnings on for the pattern analysis.
