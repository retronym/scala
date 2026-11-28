# Review campaign-1: WJ1, WL1, WL2, WL3

All four reproduce unchanged on w4-int, w3-base and 2.13.18; WL1 and WL3 also on 2.12.20.

Verdicts: 4 real, none known. Severity: WL1 medium, WL3 medium, WJ1 low, WL2 low.

Most important for the owner:

- WL1 is worse than reported. Besides the NullPointerException from snapshot() and readOnlySnapshot() (and clear()) on a read-only TrieMap snapshot, every other mutator (put, update, remove, getOrElseUpdate, ++=, filterInPlace, mapValuesInPlace) spins forever at 100% CPU: GCAS always aborts on a read-only map and the insert retries. Reaching it needs a cast or a `case m: mutable.Map` test, since readOnlySnapshot() is typed collection.Map. No library method snapshots a read-only receiver; iterator, size, isEmpty, keySet, values, view and clone all work. Fix: readOnlySnapshot returns this, snapshot copies without RDCSS, mutators throw UnsupportedOperationException. It fits retronym/scala PR 677, which already guards its calls with nonReadOnly.
- WL3: Java callers probing a Set<Integer>/Map<Long, V> view of BitSet, IntMap, LongMap or TreeSet/TreeMap[Int|Long] with null get key 0's answer; add/put of null also write key 0 (not in the finding). The unboxing is in the Scala collection's erasure bridge (unboxToInt/unboxToLong(null) == 0), not in the wrapper's cast. The hunter's fix sketch (null means false) is wrong because Scala sets and maps may hold null; this is a small design question.

WJ1: same bail-out in JavaParsers.annotation as GJ3 (PR 757) and J5 (PR 288). One fix covers all three: parse element values with constantExprOpt plus class literals through the type parser, which accepts every element value JLS 9.7.1 allows. Extend 757 with WJ1's cases and let it replace 288. User loss is small: scalac reads no Java-source annotation that takes a primitive or array class literal; @FunctionalInterface, @SafeVarargs and nullability annotations do nothing in Scala 2; run-time reflection reads javac's class files. Only macros or plugins inspecting Java symbols in a mixed build see the difference; sibling annotations are kept.

WL2: not by design per the docs (Seq.apply and mutable.Seq.update declare @throws IndexOutOfBoundsException; accumulator Scaladoc is silent). No ordinary library method writes through update on an accumulator, so a lost write needs the caller's own off-by-one.

No doubtful, by-design or wrong verdicts. No duplicates: HC4 (same wrapper methods, cooperative ==) and LM7 (Buffer.asJava.remove, handles null correctly) are other causes.
