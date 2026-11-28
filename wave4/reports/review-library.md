scala/scala · claude/eager-jackson-1fc898 · local only (uncommitted: wave4/reviews/{GA1,GC1..GC7}.json, wave4/reports/review-library.md) · PR none · unverified: 2.12 behaviour (its runner does not start on JDK 21), the GC7 matrix beyond the 11 repro lines

All eight reproduce exactly on the integration build, the base and the 2.13.18 jar.

Verdicts: 6 real (GC1 to GC6), 1 real and known (GC7, scala/bug#12121), 1 duplicate (GA1 of GC7). No doubtful, by design or wrong. Severity: 2 high (GC7, GA1), 3 medium (GC1, GC2, GC6), 3 low (GC3, GC4, GC5).

Most important:

- GC6, mutable.HashMap/LinkedHashMap.updateWith: real, a regression in 2.13.1 (scala/scala#8073); 2.13.0 stored the value. The Scaladoc does not forbid a function that modifies the map. java.util.HashMap.compute throws ConcurrentModificationException for the same function; Scala absorbs it. Worse than reported: when the function removes the key and returns None, size becomes -1 and other entries vanish from iteration. Fix is a design question (re-find the node, or reject).
- GC1 and GC2, Range and NumericRange iterator knownSize: understated. `(Int.MinValue to Int.MaxValue by (1 << 30)).iterator.toList` is empty, as is the Long equivalent. Since 2.13.0. One pull request for both.
- GC7 with GA1: one finding, GC7 primary (its matrix contains every GA1 cell). #534 covers ArrayBuffer insertAll/prependAll. #548 covers ArrayDeque insertAll and patchInPlace at 0, leaving addAll, patchInPlace elsewhere and subtractAll. #538 covers only the buffer itself, leaving every view and iterator of an UnrolledBuffer. #535 is tests only and omits HashMap, LongMap, AnyRefMap, the Java wrappers and all subtractAll. Outcome: widen #538 and #548; sets and maps wait on the #12121 decision. ArrayBuffer.flatMapInPlace is a separate mechanism and wants its own pull request. Set subtractAll is wrong only for mapped sources, not for filtered ones.

Low: GC3 (add a guard to #558; also affects an exhausted iterator of a range ending at Int.MaxValue), GC4 (widen #555), GC5 (BigDecimal ranges within 68 elements of Int.MaxValue; predates 2.13.4).

Hunter errors: GC2's claim that Byte NumericRanges are ordinary; GA1 prints fail-fast exceptions as FAIL.
