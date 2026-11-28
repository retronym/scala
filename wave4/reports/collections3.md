# collections3: coverage-chosen ordinary usage, Range model

The track was re-steered part way: the self-aliasing matrix was dropped and the Range work put on the same footing as other candidates, chosen by what two rounds of the earlier harness left unreached. This file reports the coverage-chosen rounds first, then what the Range model and the matrix had already produced.

## Ordinary usage chosen by coverage

Harness: `wave4/harness/collections3/ordinary/` (`OrdDiff.scala`, `gen_ops.py` which writes `SeqLikeOps.scala`, `cov.py`). Four families, all with in-range arguments and sizes from 0 to a few thousand (Vector up to 32^4 + 40):

- seq: one list of about 110 sequence operations instantiated three times, for `Array[T]` (ArrayOps), `String` (StringOps) and immutable and mutable `ArraySeq[T]`, with T over Int, Long, Byte, Short, Char, Double, Float, Boolean, String and Unit; each call is compared with the same call on a `List`. Also LazyZip2/3/4, ArrayBuilder, steppers and Java streams, and `mutable.StringBuilder` against `java.lang.StringBuilder` in programs of 12 edits.
- parse: `toIntOption`, `toLongOption`, `toShortOption`, `toByteOption`, `toDoubleOption`, `toFloatOption`, `toBooleanOption` against `java.lang` parsing, on numbers near each type's bounds, hex floats, exponents, Unicode digits and one-character mutations of these.
- vector: programs of one to six calls (35 kinds: builders fed chunks from eight kinds of source, appendedAll, prependedAll, concat, slices, patch, iterators after partial consumption, flatMap, sorted) against an `Array[Int]` model using `System.arraycopy`; sizes cluster around 32, 1024, 32768 and 1048576.
- hash: immutable HashMap and HashSet with keys of which a third share 50 hash codes, about 45 bulk operations (concat, merged, removedAll with five kinds of argument, filter, partition, keySet algebra, builders reused after `result`, union, diff, intersect, subsetOf) against TreeMap and TreeSet keyed by the Int; the second operand is equal to the first, derived from it by a few changes, a filtered part of it, or independent.

Lines reached in the chosen files (JaCoCo 0.8.13, integration build). "before" is the merged data of the two earlier rounds (`spike/cov/r2-all.exec`); "round1" and "round2" are this harness alone; "before+both" merges everything.

| file | before | round1 | round2 | before+both | total |
|---|---|---|---|---|---|
| immutable/Vector.scala | 836 | 1181 | 1196 | 1227 | 1625 |
| immutable/VectorMap.scala | 97 | 0 | 0 | 97 | 130 |
| ArrayOps.scala | 96 | 425 | 484 | 484 | 618 |
| StringOps.scala | 32 | 288 | 359 | 359 | 477 |
| StringParsers.scala | 0 | 139 | 139 | 139 | 139 |
| immutable/HashMap.scala | 965 | 1128 | 1129 | 1189 | 1300 |
| immutable/HashSet.scala | 941 | 1021 | 1021 | 1024 | 1147 |
| immutable/ChampCommon.scala | 80 | 80 | 80 | 80 | 94 |
| LazyZipOps.scala | 13 | 78 | 220 | 220 | 235 |
| immutable/ArraySeq.scala | 165 | 292 | 351 | 355 | 389 |
| mutable/ArraySeq.scala | 43 | 124 | 124 | 127 | 194 |
| mutable/ArrayBuilder.scala | 73 | 210 | 210 | 210 | 225 |
| mutable/StringBuilder.scala | 5 | 28 | 64 | 64 | 102 |
| immutable/Range.scala | 63 | 47 | 47 | 68 | 276 |
| immutable/NumericRange.scala | 0 | 0 | 0 | 0 | 235 |

Round 1 (seq 40 s, parse 20 s, vector 40 s, hash 40 s): 10,784 seq cases with 1.03 million checks, 2.08 million strings with 18.7 million checks, 4,046 vector programs with 55,048 checks, 396 hash cases with 17,028 checks. Signatures: 0.

Round 2 added about 25 operations to the shared list (widening appends, slices, search, sizeIs, LazyZip3 and LazyZip4 methods and their Iterable conversion, unzip, flatten, view chains), the StringBuilder programs, steppers and streams, format/compare/regex on strings, and nine Vector steps. Seq 60 s, vector 50 s, hash 40 s: 18,391 seq cases with 2.25 million checks, 5,411 vector programs with 75,789 checks, 370 hash cases with 15,910 checks. Signatures after fixing the oracle: 0.

False alarms in round 2, all the oracle's: `search` on a sequence with duplicates may return any matching index (33 signatures from this one cause); a Float stream is a DoubleStream; the StringBuilder program called `charAt` on an empty builder. They also show the comparison is live. The hash family is slow (about 10 cases per second) because a third of the cases have up to 30,000 entries.

Result: ordinary usage of ArrayOps, StringOps, StringParsers, LazyZipOps, ArraySeq, ArrayBuilder, Vector and the CHAMP maps and sets found nothing on the integration build, while raising the reached lines of those files from 3,169 to 5,398 of 6,451 (Vector, ArrayOps, StringOps, StringParsers, HashMap, HashSet, LazyZipOps, both ArraySeq, ArrayBuilder, StringBuilder). Not reached: Vector6 and the deepest VectorSliceBuilder paths (need more than 33 million elements), about 130 lines each of ArrayOps and StringOps, mutable.LongMap, IntMap, immutable LongMap, CollisionProofHashMap, TrieMap snapshots, TreeSeqMap, PriorityQueue, OpenHashMap, which were not attempted.

## Self-aliasing matrix (dropped)

Before the re-steer the matrix had run: 1,312 cells (class, bulk method, source derived from the receiver) against "the argument is read as it was before the call". Integration build: 780 ok, 222 ConcurrentModificationException, 173 silently different, 28 other exception, 7 no termination, 102 not applicable; against 2.13.18 only 30 buffer cells differ, 28 of them wrong there and ok on the integration build, and no Set or Map cell differs. The cells are in `wave4/harness/collections3/alias-integration-build.tsv`; it is the measure of GC7, and its re-entrant cells produced GC6.

## Range and NumericRange model

`RangeDiff.scala`: a BigInt model `(start, end, step, inclusive)` with a window, never materialising a long range; 595,014 ranges and 29.5 million checks in the nine runs whose logs were kept. After quarantining the five root causes below, a 60 s run of 482,467 ranges reported only GC4 and GC5. Oracle faults fixed on the way: BigDecimal sums wrapped to artificial bounds; a zero step is rejected on first use by NumericRange, not at construction; min, max, tail and init of an empty range throw NoSuchElementException; `Numeric.IntIsIntegral` as the ordering sends `Range.min` down the linear path; `dropRight` and `slice` of a NumericRange copy; `Iterator.slice` on a Range iterator steps one element at a time; NumericRange[Char].reverse throws by design.

## Findings

Each is a directory under `wave4/findings/` with a reproduction and `cmd.sh`; every one prints the same on the integration build, the base build and 2.13.18. Scala 3.7.4 was not run.

- GC1: a Range iterator's knownSize overflows when last - start does not fit in Int; toVector, to(ArrayBuffer), toList and mkString on the iterator silently drop elements. Prior: none for knownSize (LI7 is the same class's drop).
- GC2: a NumericRange iterator computes knownSize in the element type; -13 for a 38-element Byte range, 0 or 1 for wide Short, Int and Long ranges. Prior: none for the iterator (LI2, LI3 family).
- GC3: drop on the iterator of an empty Range makes it non-empty; indexWhere returns a position and segmentLength walks 2^32 elements. Prior: LI7, whose fix is in the integration build and does not cover it.
- GC4: Range.lastIndexOf(elem, end) and indexOf(elem, from) return a negative index other than -1 when the position exceeds Int.MaxValue. Prior: LI1.
- GC5: NumericRange.count throws for a BigDecimal range that crosses zero from a fractional start and has within about 2 * start / step of Int.MaxValue elements; take(Int.MaxValue) of a longer range is unusable. Prior: none found.
- GC6: mutable.HashMap.updateWith and LinkedHashMap.updateWith return Some(v) and store nothing when the function removes the key or clears the map. Prior: none found.
- GC7: open class, bulk operations fed a lazy source derived from the receiver: addAll and subtractAll of a mapped view on sets and maps, ArrayBuffer.flatMapInPlace returning a view of the buffer. Measure: the 1,312-cell matrix. Prior: LB5, LB6, LM2, LM3.

Not recorded as findings:

- Range.sorted with a step of Int.MinValue returns an empty range: it calls reverse, which is LN5 and still fails on the integration build.
- updateWith on ConcurrentHashMap.asScala returns Some(7) and leaves the old value when the function puts 64 other keys: the wrapper calls `compute`, whose contract forbids modifying the map from the function (the misuse behind KT1).
- Under-specified behaviour: min, max, tail and init of an empty Range throw NoSuchElementException where other sequences throw UnsupportedOperationException; NumericRange[Char].reverse throws ArithmeticException; in-place methods whose function reads the receiver (mapInPlace, filterInPlace, mapValuesInPlace) differ from the snapshot model in most classes; 222 matrix cells throw ConcurrentModificationException; TrieMap.getOrElseUpdate keeps a value the default expression itself put.
- Performance only: Iterator.slice, and drop after take, on a Range iterator step one element at a time.

## What the generators do not produce

Ordinary usage: mutation of the mutable ArraySeq (sortInPlace, mapInPlace), Vector beyond about 3 million elements, structural-sharing checks by identity (only by result), serialization, the families listed as not attempted. Range: BigDecimal beyond 34 digits or with a small MathContext, custom Integral instances, steppers, two-range operations. No shrinking in either harness; failures are grouped by signature with one to three examples.

## Longer campaign

Ordinary usage on these files: no for the files already above 85% (StringParsers, LazyZipOps, ArrayBuilder, HashMap, HashSet, immutable ArraySeq); two rounds, 22 million checks and no signature. The same harness shape is cheap to point at the files not attempted (mutable.LongMap, IntMap, CollisionProofHashMap, TrieMap snapshots, TreeSeqMap, PriorityQueue, OpenHashMap: add them as receivers of a shared map operation list, as was done for sequences), and that is where the next round should go. Range: no; the remaining yield is the BigDecimal precision corner. What did find defects in this track was state at Int extremes in iterators (GC1 to GC3) and re-entrancy (GC6), not ordinary usage.
