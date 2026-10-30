# Scala 2.13 collections review

Target: `2.13.x` HEAD (71440b9225). Reviewed: immutable `HashMap`/`HashSet` (CHAMP), `TreeMap`/`TreeSet`/`RedBlackTree` (immutable and mutable), `Vector`, plus a sweep of the remaining collections. Every item marked **confirmed** was reproduced on 2.13.17 via scala-cli and/or on a `build/quick` of this tree (the files in question are identical to 2.13.x there). Repro sources and fuzzers live under the session scratchpad (`.../scratchpad/{hash,tree,vector}/`); the fuzzers are worth lifting into `test/scalacheck`.

Line numbers refer to 2.13.x sources.

## Summary for a busy reader

Severity markers: 🔴 silent wrong results or corruption · 🟠 real bug or significant perf/API problem · 🟡 minor, edge case or perf nit · ⚪ cosmetic or message/docs-only.

The three core data structures are sound. Randomized differential fuzzing with deep invariant checkers (CHAMP canonical form and cached hashes; red-black colour, black height and per-node `count`; InlineVector vs List and vs 2.13.17) found no corruption in any of them. The real bugs are at the edges: builders, fast-path dispatch, mutable range views, and the interaction of `Ordering`/cooperative equality with structural fast paths.

Highest-value fixes, in order:

1. 🔴 `MapBuilderImpl.getOrElse` tests the wrong flag, so `VectorMap`/`TreeSeqMap` builders emit duplicate keys after `clear()`. One-line fix.
2. 🔴 Mutable `TreeMap`/`TreeSet` range projections: `foreachEntry`, `minAfter`/`maxBefore`, the steppers and `mutable.TreeSet.from(projection)` all ignore the bounds. Same root cause as open scala/bug#12808.
3. 🟠 `HashMap.merged` has a dead `that.size == 0` branch (should be `== 1`), making `big.merged(singleton)` O(n): 579 ms vs 0 ms on 1M entries.
4. 🟠 `NewVectorIterator.drop` overflows `oldpos + n`; `it.next(); it.drop(Int.MaxValue)` yields a negative `knownSize` and `toVector` returns the whole vector. `Vector6.appended` wraps the length negative at `Int.MaxValue` with no error.
5. 🟠 `List(...) ++: bigVector` (any unknown-size prefix) copies the whole vector: ~230x slower than a Vector prefix and O(n) memory. `VectorMap.removed` never compacts tombstones, so FIFO/LRU use leaks.
6. 🟠 `TreeSet.from`/`TreeMap.from` on unsorted input use the persistent `update` per element instead of the mutable builder: 10-19x more allocation than `newBuilder`.
7. 🟠 `LazyZip2`'s filtered view has `isEmpty = iterator.hasNext` (inverted); `mutable.Buffer.asJava` throws `UnsupportedOperationException` from most `java.util.List` mutators; `ListMap.tail`/`filter`/`map` are quadratic (seconds at 20k entries).

---

## 1. Vector and VectorMap

### Verification done
Randomized differential walk vs `ArraySeq`/`List` with structural validation (`VectorUtils.validate` plus prefix/suffix/data length invariants) after every step: 30 seeds x 21 start sizes crossing every level boundary (0..2, 31..34, 63..65, 1023..1025, 32767..32769, 2^20-1..2^20+1), vectors built via builder/`from`/append loop/prepend loop/slice/concat; ops: appended, prepended, appendedAll/prependedAll with Vector/List/ArraySeq/known- and unknown-size iterators/counted views/big slices/ArrayBuffer at sizes 0..3, 32, 33, 1000; updated, slice/take/drop/takeRight/dropRight/splitAt/tail/init, patch, `map(identity)` slice sharing, filter, builder reuse and `alignTo`, iterator drop/take/slice/copyToArray after 0-2 `next()`s, stepper `trySplit`, reverseIterator, equals/hashCode. 2.2M checks, 0 failures; every view argument evaluated exactly once on every concat path. VectorMap fuzzed against ListMap: 5.3M checks, no content or ordering differences.

### Confirmed bugs
- 🟠 **`Vector6.appended` wraps `length0` negative at `Int.MaxValue`** (Vector.scala:1061; the `suffix1.length < WIDTH` branch has no capacity check). A length-`Int.MaxValue` vector is buildable through public API in ~80 MB by structural doubling (`x ++ x` shares arrays, then `x ++ x.take(Int.MaxValue - x.length)`). Then `(y :+ "z").length == -2147483648`, `isEmpty` false, `toString` "Vector()", empty iterator. `Vector6.prepended` and `appendedAll0`/`prependedAll0` (:1138, :1153 via `append1IfSpace`/`prepend1IfSpace`) have the same arithmetic. Low severity, not on scala/bug.
- 🟠 **`NewVectorIterator.drop` overflow** (Vector.scala:2359). `mmin(oldpos + n, totalLength)` overflows. `Vector.range(0,100).iterator; it.next(); it.drop(Int.MaxValue)` gives `knownSize == -2147483548` and `toVector` (:2419, same arithmetic) returns all 100 elements; `hasNext`/`toList`/`size` happen to be right. Fix: `if (n >= totalLength - oldpos) totalLength else oldpos + n`. Not on scala/bug.
- 🟡 **`VectorMap.last` on empty throws `UnsupportedOperationException`** (VectorMap.scala:175) where the `IterableOps` contract and every other collection say `NoSuchElementException` (`head` already does).
- ⚪ **`Vector.empty.last` throws `NoSuchElementException("empty.tail")`** (Vector.scala:295).
- ⚪ **`VectorBuilder.alignTo` message says call `.reset()`** (:1576); the method is `clear()`.

### Inconsistencies
- 🟡 Overflowing `Int.MaxValue` fails five different ways by path: silent wrap (`:+`); bare `IllegalArgumentException` (`+:`, Vector6 concat, :1068/:1078); `IllegalArgumentException("advance1(-2147483648, -32): a1=[Ljava.lang.Object;@...")` dumping array identity hashes (`big ++ List`, :1847); `VectorBuilder.addOne` accepted then `result()` throws IOOBE "negative size" (:1896) while `knownSize` is already negative; `"exceeding 2^31 elements"` (`addArrN`, :1798). One message raised in `addOne`, `advance1` and the Vector6 ops would do.
- 🟡 Prepend is not symmetric with append near capacity: `y.drop(5) ` (length `Int.MaxValue - 5`) then `+:` throws once `data6` reaches `LASTWIDTH-2`.

### Performance
- 🟠 **`prependedAll` with an unknown-size prefix copies the whole vector** (:200, `k < 0` -> `super.prependedAll`, unaligned builder -> `foreachRec(addArr1)`). On 2^20 elements: `List(1,2,3) ++: big` 21.7 ms vs `Vector(1,2,3) ++: big` 0.09 ms (~230x), and O(n) memory (OOMs on a sparse 2^31 vector). `appendedAll` with `k < 0` is fine because `initFrom(this)` shares. Fix: materialise the prefix first, then `prependedAll0(p, p.length)`. High value, easy.
- 🟡 **`drop(1)`/`dropRight(1)` are 5-8x slower than `tail`/`init`** (734 ns vs 88 ns; 650 vs 156 at 2^20) because `drop` is `slice(n, length)` (:253) and always goes through `VectorSliceBuilder`. Give `lo < len1 && hi == length` (and the suffix mirror) a `copyOfRange(prefix1)` path.
- 🟡 **`patch` is not overridden**: `StrictOptimizedSeqOps.patch` rebuilds, 32 ms for a one-element patch at 2^20 vs 1 us for `updated`. `take(from) ++ other ++ drop(from + replaced)` would reuse the aligned concat.
- 🟡 `prepend1IfSpace` case 1 (:2194) uses the reflective `copyPrepend` (`Array.newInstance`, :2106) on an `Arr1` where `copyPrepend1` would do; same reflective path on every `VectorN.prepended` rollover and in `mapElemsRest`.
- 🟡 The tiny `prependedAll0` path (:216) allocates a whole Vector via `IndexedSeq.from(prefix)` just to get a `reverseIterator`.
- 🟡 `VectorN.tail`/`init` never drop a dimension: a `Vector2` drained by `tail` stays a `Vector2` at 1 element (704 such vectors in the walk). Normalise when `length0 <= WIDTH`.
- 🟡 Big concat is O(min(n,m)/32) when aligned and O(n) element-wise otherwise (unaligned `half ++ half` on a sparse 2^30 vector OOMs). Known design (scala/bug#7725) but undocumented.
- 🟠 **`VectorMap.removed` never compacts** (VectorMap.scala:125-166 writes tombstones; only `tail`/`init` trim). LRU-style `m.removed(m.head._1).updated(i, i)` x 10^6 leaves `size == 100` with `fields.length == 1,000,100`. Compact when `fields.length > 2*size + c`. Not on scala/bug (scala/bug#11326 is about bulk ops).
- 🟡 VectorMap iteration is ~4x slower than ListMap (427 ms vs 104 ms for 100k x 10): `nextValidField` (:72) allocates an `(Int, K)` tuple, pattern-matches Tombstone, then does a `HashMap` lookup per element; `values` also looks up per key.

### Footguns
- 🟡 `Vector.from(ArraySeq.unsafeWrapArray(arr: Array[AnyRef]))` with 1..32 elements shares `arr` (:47; intentionally tested by `factoryReuseArraySet`), undocumented on `Vector.from`.
- 🟡 `xs ++: v` with a List or Iterator prefix is O(n) (above).
- 🟡 `VectorMap.keys` is a `Vector`, so `SeqMap(kvs*).keys` (a `Set` for n <= 4) `!= VectorMap(kvs*).keys` at small sizes and `HashMap.keys != VectorMap.keys` at every size (scala/bug#12991, open). Binary-incompatible to change in 2.13; document.
- 🟡 `Vector[Int]` boxes; the class doc gives no Vector vs ArraySeq guidance.

### Checked and fine
Shared `Vector.emptyIterator` (:91, :131): every mutation on it is idempotent (`drop`/`take`/`slice` write zeros, `copyToArray` copies nothing), sharing is unobservable. Legacy `VectorIterator`/ParVector hooks (`startIndex`/`endIndex`/`initIterator`, :312, :2477): constructor args ignored, `endIndex` dead. Other overflow sites (`slice`/`takeRight`/`dropRight` at `Int.MaxValue`, `iterator.take(Int.MaxValue)`, stepper `estimateSize`, `lengthCompare`, `updated(Int.MaxValue-1)`, `VectorSliceBuilder` `count`/`pos`, `advanceSlice`) correct. `fill(-5)`/`tabulate(-3)` empty. Builder reuse after `result()` without `clear()` at the 32/33 boundary. VectorMap: `updated` of existing key keeps position, builder `aliased` path, tombstone runs straddling a `tail`-dropped prefix, null keys; scala/bug#11217 and scala/bug#11220 closed and don't reproduce.

### Docs
- `VectorBuilder` (public, :1397) has no Scaladoc: document reusability, that `addAll(v: Vector)` on an empty builder is O(log n) and shares arrays, and the `Int.MaxValue` limit and what is thrown.
- Class Scaladoc (:95-115) mixes user guidance with internals (prefix1/suffix1 balancing). Move internals to a code comment; add concat cost, `drop(1)` vs `tail`, `patch` O(n), prefer a builder for bulk construction, when to use `ArraySeq`.
- `fillSparse` "TODO Make public" (:73-74) still pending.
- VectorMap class doc (:21): add that `removed` leaves tombstones and `keys` is a `Vector`.

### Test gaps
No `Iterator.drop` near `Int.MaxValue` after advancing; no capacity test at `Int.MaxValue` (cheap via `fillSparse`, visible to junit in `scala.collection`); no message pin for `Vector.empty.last`; no Vector ScalaCheck model test at all (`test/scalacheck` has List, Range, Map); `VectorMapTest` has no churn-memory, empty-`last` or `keys`-equality test.

## 2. immutable.HashMap / HashSet (CHAMP)

### Verification done
2000-seed differential fuzz with keys whose *improved* hashes are crafted (inverting `Hashing.improve`) to force depth-7 prefixes and full collisions (~1.1M collision-node visits). After each op: canonical form (subnodes size >= 2), data positioned by mask, `originalHashes == key.##`, recomputed `size` and `cachedJavaKeySetHashCode`, iterator/reverse/foreachEntry counts, `hashCode == MurmurHash3.mapHash/setHash`, symmetric `equals` vs shuffled rebuild and vs ListMap/ListSet, and all previously published versions unchanged (no builder/shallow-mutation aliasing). No failures. The in-place `updateWithShallowMutations`/`migrate*` paths never mutate a shared `originalHashes` array.

### Confirmed bugs
- 🔴 **`MapBuilderImpl.getOrElse` tests `hashMapBuilder ne null` instead of `switchedToHashMapBuilder`** (Map.scala:666). After a builder has switched (>4 keys) and been `clear()`ed, lookups go to the empty hash builder while entries go to `elems`. `VectorMapBuilder.addOne` (VectorMap.scala:265) and `TreeSeqMap.Builder.addOne` (TreeSeqMap.scala:325) dedup through it, so `b.clear(); b += 1->"b"; b += 1->"b"` gives `VectorMap(1 -> b, 1 -> b)` with `size == 1` and 2 iterated entries; `removed(1)` then empties it. Medium severity, not on scala/bug, untested.
- 🟠 **`HashSet.removedAll(r: Range)` depends on `r.length > size`** (HashSet.scala:310). The long-range path filters with `case i: Int => !range.contains(i); case _ => true`, so `1L`, `2.0`, `'a'`, `BigInt(3)` survive; the short path removes them via cooperative `==`. `HashSet[Any](1L,2.0,'a',3).removedAll(1 to 3)` is `HashSet(a)`, `.removedAll(1 to 100)` is `HashSet(1, a, 2.0)`. Also `removedAll(0 to Int.MaxValue)` throws from `Range.length`. Drop the special case or restrict it.
- 🟠 **`HashSet.diff`/`removedAll(that: Set)` semantics depend on relative size** (:255-267). `thatKnownSize <= size` iterates `that` and excludes by `==`; otherwise `filterNot(that.contains)`. With a case-insensitive `mutable.TreeSet`, `HashSet("a","B","c") diff TreeSet("A")` keeps `a`, `diff TreeSet("A","x","y","z")` drops it. `Set3`, `intersect` and `subsetOf` consistently use `that.contains`. Take the iterate-`that` path only for known hash-based sets.
- 🟠 **`HashMap.merged` dead fast path** (HashMap.scala:356). `else if (that.size == 0)` can never hold after the `that.isEmpty` check at :341; intended `== 1`. `big.merged(one)` 579 ms vs `one.merged(big)` 0 ms at 1M entries. Note the dead body is also wrong: reads `rootNode.getPayload(0)` from `this` not `that`, and must keep `mergef((thisK,thisV),(thatK,thatV))` order. Introduced in 1e5fd09ef3.

### Performance
- 🟡 `HashSet.diff(HashSet)` computes `rootNode.diff` twice (:224-225; use `newRootNode`). Exactly 2x equals calls and defeats the `eq` short-circuit.
- 🟡 `get`/`apply`/`getOrElse`/`removed` (HashMap.scala:670, 685, 717, 871; HashSet.scala:611, 662) call `equals` on a slot hit without comparing the cached `originalHashes(index)` first, unlike `containsKey`/`contains`. Cheap win for expensive `equals`.
- 🟡 `updated(equalKey, sameValue)` allocates a new path because the test is `key0 eq key && value0 eq value` (:753, :803) while `copyAndSetValue` never stores the new key (:962 commented out). Test value identity only.
- 🟡 Builder `result()` then add deep-copies the whole trie: alternating add/result is O(n^2) (0.9 s / 3.2 s / 12.9 s at 5k/10k/20k). By design, but `newBuilder` docs advertise reuse without the cost.
- 🟡 `HashMap.removedAll(HashSet)` is O(m log n) with `##` recomputed per key (TODO at :396); `removedAll(otherMap.keySet)` likewise.
- 🟡 `intersect`, `partition`, `take*`/`drop*`, `span` are "preemptively overridden" (2019) but delegate to super; `intersect` filters the receiver even when `that` is tiny. `HashKeySet.filter` (:74) goes through `HashMap.filter`, allocating a tuple per entry. `HashSetBuilder.addAll` lacks the `mutable.HashSet` stored-hash fast path that `HashMapBuilder` has.

### Footguns
- 🟠 **Which key instance survives is inconsistent** (scala/bug#10415 discussed "existing key retained"). Old key kept by `updated` (bitmap node), `++ List`, builder on bitmap nodes, `HashSet +`. New key wins in `HashCollisionMapNode.updated` (:1915), builders on collision nodes (:2318, :2046), `HashMap ++ HashMap`. `HashSet ++ HashSet`: `leftDataRightDataLeftOverwrites` (HashSet.scala:1668) writes the right element and does not set `anyChangesMadeSoFar`, so which survives depends on whether other slots changed. `HashSetBuilder.update` does a pointless `setValue(bm, bitpos, element0)` (:2024).
- ⚪ `HashMap.empty.head/last/tail` and `HashSet.empty.last` throw "next on empty iterator". `HashSet.reverseIterator` (:61) lacks the `isEmpty` guard `HashMap`'s has.
- 🟡 `merged` exists only on `HashMap`, not on `Map1..4` (scala/bug#11722, open).

### Docs
- HashMap.scala:32 `@tparam K ... this hash set` -> "hash map".
- Class docs state nothing about complexity ("effectively constant: depth <= 7 plus collision scan"), iteration order (unspecified, deterministic per key set), or key retention. `head`/`last`/`tail`/`init` are O(log n); say so.
- `newBuilder` docs: add "adding after `result()` first copies the structure, O(n)".
- `merged` `@param mergef` recommends passing `null`; instead document the call shape `mergef((k, thisV), (k, thatV))`.
- Stale "preemptively overridden ... may be included in a minor release" comments on `updatedWith`, `partition`, `take*`, `span`.

### Test gaps
No clear-then-reuse tests for `VectorMap`/`TreeSeqMap` builders; nothing on `removedAll(Range)` with non-Int numerics; nothing pins `merged` with a singleton to the fast path; nothing on key identity. The existing ScalaCheck generators (`Int`/`CustomHashInt`) rarely reach depth 7 or collision nodes; the hash-inverting generator from the fuzzer fixes that.

## 3. TreeMap / TreeSet / RedBlackTree

### Verification done
4000-seed differential fuzz (1.14M checks) and exhaustive stress for n <= 300 over incl/excl/updated/removed/updatedWith, concat, intersect/diff/removedAll, all range ops, take/drop/slice/splitAt/takeWhile/dropWhile, tail/init, filter/partition, transform, builders with mixed `++=`. Checker: root black, BST order, no red-red, equal black height, **every node's `count` equals true subtree size** (catches leaked mutable builder nodes), height <= iterator stack capacity, equals/hashCode vs HashSet/HashMap and vs rebuild, persistence of operands. No failures in the immutable core. Files unchanged since 2.13.17 apart from doc style.

### Confirmed bugs
- 🟠 **`TreeSetBuilder.addAll` has a `case ts: TreeMap` that adds the map's keys** (TreeSet.scala:281-283). Matches only when `A` is a supertype like `Any` and the orderings are `==`; then `(TreeSet.newBuilder[Any](o) ++= tm).result()` is `TreeSet(1, 3)` instead of the pairs, reachable via `flatMap`. Delete the case (`TreeMap.keySet` is already a `TreeSet`).
- 🔴 **Mutable `TreeMap`/`TreeSet` range projections ignore bounds in inherited overrides.** Same root cause as open scala/bug#12808 (`clear()`):
  - `foreachEntry` (mutable/TreeMap.scala:126): `m.rangeFrom(3).foreachEntry` visits 1,2,3,4.
  - `minAfter`/`maxBefore` (mutable/TreeMap.scala:138-140, mutable/TreeSet.scala:105-107): `m.rangeFrom(3).minAfter(0) == Some((1,a))`.
  - `stepper`/`keyStepper`/`valueStepper` (mutable/TreeMap.scala:79-111, mutable/TreeSet.scala:56-66) pass the projection's size but the whole root: `rangeFrom(3).keyStepper` yields `1, 2`. Affects `asJava*Stream`.
  - `mutable.TreeSet.from(projection)` (mutable/TreeSet.scala:197) copies the whole tree: `s.rangeFrom(4).to(mutable.TreeSet)` is `TreeSet(1..5)`.
  - `immutable.TreeMap.from(projection)` is correct (checked), so iterator-based paths are fine. Audit every override in the parents.
- 🟠 **Sorted `equals` uses `ordering.equiv` but `hashCode` uses element hashes** (TreeSet.scala:223, TreeMap.scala:295, collection/SortedSet.scala:30, SortedMap.scala:31). Case-insensitive ordering: `TreeSet("a") == TreeSet("A")` true, hashCodes differ, `TreeSet("a") == Set("A")` false but `Set("A") == TreeSet("a")` true. Map side is scala/bug#12228 (closed, won't fix); undocumented.

### Suspected
- 🟡 Iterator stack capacity `2*ceil(log2(n+2)) - 2` (RedBlackTree.scala:849) is exactly tight: stress hit height == capacity (n=6 h=4, n=14 h=6). Correct for valid trees since all producers `blacken`, but zero slack means any future red-root leak becomes an AIOOBE inside iteration rather than a validation failure. Consider +1 or a comment at the `blacken` sites.
- 🟡 `mutable.TreeSet.from` Range case (mutable/TreeSet.scala:201) tests `ordering eq Ordering.Int.reverse`, which allocates a fresh `Reverse`, so never true. Perf only.

### Performance
- 🟠 **`TreeSet.from`/`TreeMap.from` unsorted path uses persistent `RB.update` per element** (TreeSet.scala:253-257, TreeMap.scala:317-324). 200k random Ints: 122.5 MB vs 6.4 MB via `newBuilder` (19x); map 128.9 vs 12.8 MB (10x). This is the path for `TreeSet(xs: _*)`, `.to(TreeSet)`, sorted `map`/`flatMap`/`collect`/`zip`, `keySet.map`, `fromSpecific`. Route through the builder/`Adder`.
- 🟡 `TreeSet.concat(nonTreeSet)`, `TreeSet.removedAll`, `TreeMap.removedAll` go element by element through the persistent tree (`TreeMap.concat` already uses the mutable `Adder`): `ts ++ Vector(20k)` 13.4 MB vs `tm ++` 4.35 MB.
- 🟡 `rangeTo` not overridden: generic `SortedOps.rangeTo` builds `rangeFrom(to)`, iterates, then `rangeUntil`: 2x comparisons, 2.2x allocation. `RB.to`/`doTo` (RedBlackTree.scala:200, 458) exists and is dead code. `RB.nth` is also dead.
- 🟡 All fast paths require `ordering == that.ordering`. `Ordering.by`/`fromLessThan` have no `equals`, so `implicit def ord = Ordering.by(...)` defeats every one of `++`, `from`, `diff`, `intersect`, `equals`: `TreeSet(P(-1)) ++ a` (100k) allocates 80 MB, `TreeSet.from(a)` copies instead of returning `a`.
- 🟡 Deserialization rebuilds via `builder += x` (O(n log n) comparisons) though the stream is sorted; `RB.fromOrderedEntries` could do it in O(n).
- 🟡 `TreeMap` doesn't override `keysIterator`/`valuesIterator` (tuple per entry when cold); `RB.TreeIterator` lacks `drop`/`knownSize` (rank descent possible via `count`); `doFrom/doTo/doUntil/doRange` re-insert the pivot with `upd` where `join` needs no comparisons; `split`/`splitLast` allocate `Tuple3/4` per level.

### Footguns
- 🟠 Keys identified by `compare == 0`, old key retained: `TreeMap("a"->1)(ci).updated("A", 2) == TreeMap(a -> 2)`. Known (scala/bug#5986, scala/bug#12228) but undocumented.
- 🟠 `TreeMap.keySet` shares the map's tree including values; any `TreeSet` derived from it (`incl`, `filter`, union) keeps the values reachable. Verified a 1 MB array retained.
- 🟡 Mutable tree iterators are not fail-fast and can go backwards after concurrent removal: `(3,3), (2,2), (5,5)...`.
- 🟡 `TreeMap` primary constructor does not null-check `ordering` (`TreeSet` does); fails late.
- 🟡 `SortedMap.WithDefault.newSpecificBuilder` hardcodes `SortedMap.newBuilder` while `fromSpecific` uses `underlying.sortedMapFactory` (immutable/SortedMap.scala:171-175).

### Docs
- immutable/TreeMap.scala:29,31: stray `* ` renders literally before the HashMap and SeqMap links.
- TreeMap.scala:61: `@param ordering ... objects of type A` -> `K`.
- TreeSet.scala:166 `excl`: `@param elem a new element to add` -> "the element to remove".
- collection/SortedOps.scala:21-32 `rangeImpl`: "Any mutations in the ranged projection will update this collection and vice versa" is meaningless for immutable and overpromises for mutable (see scala/bug#12808 and the bugs above).
- `minAfter`/`maxBefore`: "None if there is no such node" leaks the implementation.
- Neither TreeMap/TreeSet nor SortedMap/SortedSet documents: identity is `compare == 0`; `==`/`hashCode`/`contains` may disagree with hash-based sets under a non-equals-consistent ordering; fast paths need `Ordering.==` so prefer `implicit val`. No complexity statements at all (O(log n) lookup/update/head/last/tail/init/take/drop/range, O(1) size, join-based union). Mutable docs don't mention non-fail-fast iterators or that projection `size` is O(n).

### Test gaps
`test/scalacheck/redblacktree.scala` doesn't check per-node `count`, the mutable builder/`Adder` + `beforePublish` path, or height vs iterator stack. Nothing tests projection `foreachEntry`/`minAfter`/`maxBefore`/steppers/`from`, `TreeSetBuilder ++= TreeMap`, or equals => hashCode under a non-equals-consistent ordering.

## 4. Sweep of the remaining collections

Shallower than the sections above: behaviour tests plus randomized checks, not a line-by-line read. Not reached at all: `AnyRefMap`, mutable `LongMap`, `OpenHashMap`, `UnrolledBuffer`, mutable `BitSet` at large indices, `CollisionProofHashMap`, `StringOps`, `WrappedString`, `Searching`, `Stepper`/`scala.jdk` accumulators, `Stream`. Randomized checks that found nothing: `IntMap`/`LongMap` incl. `MinValue`/`MaxValue` keys vs HashMap, `TreeSeqMap`, `ArrayDeque` (14 ops vs Vector), `PriorityQueue`, `ListBuffer` toList aliasing, Iterator sliding/grouped/span/partition/duplicate/scanLeft/padTo/zipAll/slice incl. `Int.MaxValue` args, `ConcatIterator` at 1M depth both nestings, immutable `BitSet`, Range/NumericRange edges, Java wrapper equals/hashCode/round trips.

### Confirmed bugs
- 🟠 **`LazyZip2` filtered view has inverted `isEmpty`** (LazyZipOps.scala:92): `override def isEmpty = iterator.hasNext`. `xs.view.lazyZip(ys).filter(p).isEmpty` is `true` on a non-empty result. LazyZip3/4 (:194, :224, :328) use `iterator.isEmpty` and are correct. Introduced in 874092e72f.
- 🟠 **`mutable.Buffer.asJava` is missing mutators** (JavaCollectionWrappers.scala:130-135). On a mutable `ArrayBuffer`, `add(int,E)`, `addAll(int,coll)`, `remove(Object)`, `removeAll`, `retainAll`, `removeIf`, `iterator().remove()` and `listIterator().add` all throw `UnsupportedOperationException`; `IteratorWrapper.remove` (:33) always throws. The Set and Map wrappers do support removal.
- 🟡 **`MapWrapper` entry `setValue` leaves `getValue` stale** (JavaCollectionWrappers.scala ~:260); the map itself is updated. Violates the `Map.Entry` contract.
- 🟠 **`ArrayOps.transpose` is silent on ragged input** (ArrayOps.scala:1306-1331): `Array(Array(1,2),Array(3)).transpose` is `[[1,3],[2]]`; a longer later row gives AIOOBE. `List`/`Vector` throw `IllegalArgumentException("transpose requires all collections have the same size")`.
- ⚪ **`ArrayBuffer.checkWithinBounds` reports `hi-1`** (ArrayBuffer.scala:100-103): `ArrayBuffer(1,2,3).insert(4, x)` says "3 is out of bounds (min 0, max 2)" though 3 is a valid insert index.
- 🟡 **Empty-collection exceptions are inconsistent:** `VectorMap().last` throws `UnsupportedOperationException("empty.last")` while `List`/`Map`/`ListMap` throw `NoSuchElementException` (the latter two with "next on empty iterator"). `BitSet(-1)`, `BitSet.map(_ - 2)` fail with a bare "requirement failed".
- ⚪ Already fixed on 2.13.x HEAD vs 2.13.17: `indexOfSlice` with negative `from` on non-indexed seqs (100k randomized comparison vs naive: 11908 mismatches on 2.13.17, 0 on HEAD).

### Suspected
- 🟡 `ListMapBuilder.addAll` (ListMap.scala ~292-343) treats any `collection.Map`/`MapView` as having unique keys; a `SortedMap` whose ordering is coarser than `==` could yield duplicates. Not run.
- 🟡 `JListWrapper.insertAll(i, elems)` iterating a view of the same list would CME. Not run.
- 🟡 Fail-fast is inconsistent: `ArrayBuffer.iterator.next` after `+=` doesn't throw but `toList`/`foreach` do; `ListBuffer.iterator.next` after `clear` returns a stale element (check is in `hasNext` only).
- 🟡 `asScala` on a `ConcurrentMap`: `getOrElseUpdate` is `computeIfAbsent`, so recursive use throws `IllegalStateException("Recursive update")` (confirmed), unlike `mutable.HashMap`/`TrieMap`. Null values: `get` is `Some(null)`, `getOrElse(k, d)` is `null`, `getOrElseUpdate(k, d)` is `d`.

### Performance
- 🟠 **`ListMap` subset ops are quadratic.** 20k entries on 2.13.17: `tail` 5.0 s, `filter` 9.2 s, `map` 2.9 s, `++` 2.8 s. `ListMapBuilder.addOne` scans for duplicates and the no-dedupe path in `addAll` only fires for `collection.Map`/`MapView`; `filter`/`drop`/`take`/`tail` feed a plain View. Class doc says `tail` is O(n). Either add a no-dedupe path for ops known to preserve uniqueness or fix the doc.
- 🟡 `IndexedSeq.asJava` does not implement `java.util.RandomAccess`, so `Collections.shuffle`/`binarySearch` take slow paths. `JListWrapper`/`JMapWrapper.knownSize` return -1 though `size` is O(1), losing presizing.
- 🟡 `LazyList` with a 1 MB stack: left-nested `l ++ LazyList(i)` overflows near n=8000; chains of `.map`/`.filter`/`.flatMap` near n=2000. Right-nested and straight pipelines at 1M are fine.

### Footguns
- 🟠 **`mutable.Stack += x` / `addOne` appends at the bottom** (inherited from `ArrayDeque`): `Stack(1,2) += 9` leaves `top == 1`; `pushAll(List(7,8))` puts 8 on top but `++=`, `Stack.from` and `.to(Stack)` put the first element on top. Undocumented.
- 🟡 `Iterator.size` consumes the iterator only when `knownSize < 0`; no Reuse note on `size`.
- 🟡 `var i = ...; i = Iterator.single(1) ++ i` loops forever because `concat` is by-name and captures the var.
- 🟡 `View` equality is reference equality.
- 🟡 `IntMap`/`LongMap` iterate and order `firstKey`/`lastKey` by unsigned key (negatives last); only the iterator docs say so.
- 🟡 `List(Double.NaN).contains(Double.NaN)` is false.

### Docs
- `ListMap` class doc: `tail` is O(n^2), `size` is O(n) (`knownSize == -1`), `filter`/`map`/`++` quadratic.
- `immutable.Map.withDefault`/`withDefaultValue` (Map.scala:34-54) says transformer methods "will not preserve the default value". Measured: `filter`, `drop`, `tail`, `concat`, `updated`, `removed`, `updatedWith` keep it; `map`, `transform`, `view.mapValues`, `view.filterKeys` lose it. Suggested: "Methods returning a Map of the same element type preserve the default; methods that build a new element type or a view do not."
- `mutable.Stack`: document that `+=` does not push.
- `CollectionConverters`: list unsupported mutators on `Buffer.asJava`; state that immutable wrappers throw `UnsupportedOperationException`.
