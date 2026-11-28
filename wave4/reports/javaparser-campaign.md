# javaparser campaign

Continues the spike (`reports/javaparser.md`) with the same two oracles: each Java member typed by scalac from source against the same member read from javac's class file, and a Scala probe compiled jointly with the Java sources and separately against the class files, run against javac's classes. Changes to `wave4/harness/javaparser/`: JaCoCo and a per-scope summary (`cov.py`), counted exclusions of known differences in `Driver.scala`, subpackages and Scala definitions that the Java sources refer to, and three generator features, one per round (README).

## Rounds

Round 1 ran on `w3-int`; rounds 2 to 4 on `w4-int`, which appeared after round 1. Line coverage per source file (covered/total):

| Round | Change | Cases | s | JavaParsers | JavaScanners | JavaConstantFolder | ClassfileParser | Namers | New |
|---|---|---|---|---|---|---|---|---|---|
| 1 | spike generator, coverage and exclusions added | 150 | 56 | 699/764 | 510/651 | 73/78 | 652/808 | 583/1069 | 0 |
| 2 | `Au`: annotation uses with every element type | 200 | 123 | 711/768 | 510/651 | 73/78 | 658/808 | 633/1069 | WJ1 |
| 3 | `X`: packages, imports, access, package-info, Java using Scala | 200 | 242 | 712/768 | 512/651 | 73/78 | 660/808 | 642/1069 | 0 |
| 4 | `N`: non-ASCII identifiers, compound assignments | 150 | 170 | 712/768 | 543/651 | 73/78 | 660/808 | 642/1069 | 0 |

Per-round detail is in `wave4/rounds/javaparser.json`; the treemap of `scala/tools/nsc/javac` is `reports/javaparser-treemap.html`.

False alarms and generator faults, all fixed before the counted runs: forward references to constants in annotation arguments and a parent-package constant used in a subpackage (125 javac rejects in two trial runs), a protected inner-class constructor called from another package (correctly rejected by both orders), scala-reflect missing from the probe's class path, and CLASS-retention annotations, which scalac reads from source but not from class files (the class file parser skips `RuntimeInvisibleAnnotations`, a documented TODO; the generator now uses RUNTIME).

Exclusions and what they discarded (lines): over rounds 2 to 4, record `equals`/`hashCode`/`toString` explicit only in class files 1248 and GJ1 213; over rounds 3 and 4, once their filters matched, bounded `T[]` printing 91 and WJ1 177. GJ2 stopped appearing on `w4-int`, where it is fixed.

## Findings

- WJ1 (`findings/WJ1`, area J): a Java annotation parsed from source is dropped when an argument is a class literal of a primitive type, `void` or an array type (`int.class`, `String[].class`). `JavaParsers.annotation` reads class literals only through a qualified identifier. Seen through a macro: absent in joint compilation, present in separate. Integration, base and 2.13.18 alike. Same bail-out as GJ3, but GJ3's write-up lists class literals as accepted.

## Known, met again

- J9 (wave one, a remark in retronym/scala#291): Scala cannot select a Java member whose name has a supplementary character, from source or class file, because Scala encodes the surrogate pair as `$uD835$uDC65`. It rejected 64 of 150 probes in round 4; added to `known-signatures.md`, and the generator now declares such members without using them. A 40-case rerun was clean.

## Under-specified, not recorded

A double constant folded to NaN with the sign bit set (`-(x % 0f)`): scalac folds and emits the negative NaN, while javac's `ConstantValue` holds the canonical NaN, so joint and separate builds print different raw bits for the same NaN. Found once in round 2.

## Not reached and next

Rounds 3 and 4 found no new signature. Round 3 reached the Java import checks in `Namers` and the package-info path, round 4 took the scanner from 512 to 543 lines. What remains unreached in `JavaParsers` and `JavaScanners` is almost all error handling (syntax errors, `skip`, `token2string`). Reaching it needs invalid Java inputs, and the oracle would be that both scalac and javac reject them, which the harness does not yet do. Not done in this campaign: Scala probes that must be rejected in both orders, generic signatures beyond the spike's `G`, and Scala classes and generated Java classes inheriting from each other with bridges and covariant returns (round 3 covers only overriding a Scala method from Java and a covariant `co()`). The yield pattern is the spike's: the one new signature came from the first new construct. More cases of these three families are unlikely to add one.
