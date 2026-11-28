# Campaign: programs track (general program generator)

Continues the erasure spike with `@specialized` at weight 0. Build under test: `w4-int` from round 1 (it appeared before round 1; two smoke runs used `w3-int`).

## Driver

`wave4/harness/driver/` (README there) is a shared in-process driver: one JVM holds the build under test and 2.13.18, each in its own class loader, reached through a small bridge class compiled by each compiler. Every compilation gets a fresh `Global` and writes into memory; separate compilation puts earlier units' classes on the class path in memory. All configurations of a program compile and run concurrently under a permit pool with per-compile (60 s) and per-run (20 s) timeouts. Outcomes record output, exception class, compiler errors or crash frame, and anything the compiler prints (how `-Ycheck` complaints are caught). JaCoCo is per JVM, one JVM per round.

Throughput on the same generator: the spike driver did 6.3 programs a minute (inliner configuration 73 of 95 s). The new driver does about 100 a minute in one JVM (1,000 compilations a minute), 34 under JaCoCo; with two JVMs at once, 78 to 121 plus 28 to 30 under JaCoCo. The inliner configuration now runs on 1 seed in 20, only for sources under 15,000 characters, and `Main` is split into methods of at most 12 calls. `-opt:local` still costs three to five times the reference compile, because of the handler-count cost below.

Oracles: `-opt:local` against default, `-Ydelambdafy:inline`, joint against separate compilation in dependency order, integration build against 2.13.18 (every program), `-Ycheck:all` on 1 in 10, inliner on 1 in 20. Scala 3 was not added as an oracle: the generator uses Scala 2 syntax throughout and Scala 3 needs its own bridge; it was used by hand for WP3.

## Rounds

| Round | Change | Programs | Compiled | New | Known | transform | patmat | backend/jvm |
|---|---|---|---|---|---|---|---|---|
| 1 | new driver, `@specialized` off | 130 | 100% | 0 | 0 | 3698/5328 | 1500/2186 | 2641/3905 |
| 2 | Int/Char/String switches, guards, `@switch` | 175 | 100% | 1 (WP1) | 1 (GE6) | 3746 | 1566 | 2659 |
| 3 | structural calls, `@varargs` | 390 | 99.2% | 1 (WP2) | 4 | 3897 | 1566 | 2690 |
| 4 | Byte/Short/Char/Float operands, `##`, `getClass` | 555 | 99.8% | 1 (WP3) | 5 | 3916 | 1566 | 2792 |

1,250 programs in all, about 19,000 compilations, 220 s per round. Details and largest unreached methods per round: `wave4/rounds/programs.json`; treemap: `reports/programs-treemap.html`. The stop rule was not reached; the campaign stopped at its time budget.

False alarms and generator faults, each fixed before the next round: ambiguous numeric overloads in a subclass (4% of programs; ambiguous by the overloading rules); `-Ycheck` output on a reference that did not compile; a value class as a structural result type (rejected by SIP-15 on all builds); a generic value class taken to erase to Object; `x.##: @switch` parsing as an operator.

Known differences counted and excluded: 2.13.18 throws VerifyError or ClassCastException where the integration build is correct in 68 programs over 8 signatures; the five signatures checked also fail on the base, so earlier waves fixed them. GE6 also hits `-opt:local` alone (added to `known-signatures.md`): 100 consecutive try/catch blocks in one method take 6 s in the jvm phase, 200 throw `Too many allocated bytes` from `CopyProp`'s `ProdConsAnalyzer` (2.13.18: 68 s, 25 GB allocated). retronym/scala#767 already describes this path.

## Findings

- WP1 (spurious warning): `-Ycheck:all` reports the default label jump of a guarded literal switch with a boxed result as erroneous after erasure; the code is correct. On the base and 2.13.18 the checker also aborts the compilation. It was 10 of 17 `-Ycheck` samples in round 2.
- WP2 (spurious warning): in a method of a generic value class, a structural type keeps the class's type parameter after extmethods; `-Ycheck` reports the call.
- WP3 (miscompile): a partial function with a value class result whose body is a non-switch match with a try arm fails verification; `-opt:local` crashes. Erasure boxes the value class around a try that uncurry left in place. Same family as CF3 / retronym/scala#605, different trigger; present on all three builds, absent in Scala 3.7.4.

Not triaged: two `-Ycheck` signatures from round 4 (`Tree has erroneous type: N.this.N()`, and a boxed `RichI.dbl$extension` call). Shrunk programs are in the scratch directory `w4c-programs/r4b/`.

## Not yet produced

Java-facing members (generic signatures, `@varargs` called from Java), symbol and array literals, static forwarders and `appendToStaticCtor`, string concatenation with more than 200 parts, stackable traits with `abstract override`, secondary constructors, inner classes with outer references. Shrinking still works on whole lines only.

## Longer campaign?

Yes. Each new construct produced a new signature in its first round (switches, structural calls, narrow primitives); more cases of an old construct produced none. WP3 shows what a long run is for: features introduced in different rounds (a value class, a partial function, a non-switch match and a try) combining. The driver makes that affordable at 100+ programs a minute.
