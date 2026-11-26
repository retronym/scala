A `TrieMap` obtained from `readOnlySnapshot()` (typed `collection.Map`, reached as a `TrieMap` or `mutable.Map` through a cast or a type test) handles its read-only state only on the read paths. On such a map `snapshot()`, `readOnlySnapshot()` and `clear()` throw `NullPointerException`, and every other update (`put`, `update`, `remove`, `putIfAbsent`, `replace`, `getOrElseUpdate`, `++=`, `filterInPlace`, `mapValuesInPlace`, `updateWith`) never returns.

```scala
val ro = TrieMap(1 -> "a").readOnlySnapshot().asInstanceOf[TrieMap[Int, String]]
ro.snapshot()   // NullPointerException in CAS_ROOT
ro.put(2, "b")  // spins at 100% CPU
```

A read-only snapshot has no root updater, so the two snapshot methods and `clear`, which swap the root with `RDCSS_ROOT`, dereference null in `CAS_ROOT`. The updates go through `inserthc`, `insertifhc` and `removehc`, which retry until the trie operation succeeds; on a read-only map `GCAS` always aborts, so they retry forever. The Scaladoc of `readOnlySnapshot` says the snapshot "cannot be modified"; the nearest Java read-only maps (`Collections.unmodifiableMap`, `Map.copyOf`) answer every read and throw `UnsupportedOperationException` from mutators.

Fix:

- `readOnlySnapshot()` on a read-only map returns the map itself, since it never changes (as `Map.copyOf` returns an immutable argument).
- `snapshot()` on a read-only map builds a new writable `TrieMap` over a copy of the root in a new generation, with its own root updater. No root swap on the receiver is needed: nothing writes to it, and the new map copies paths lazily as it is written, as a snapshot of a writable map does.
- The three update loops and `clear` throw `UnsupportedOperationException` on a read-only map. The check is one field test per update; `getOrElseUpdate` on a key that is present stays a read and answers.

Long-standing: the read-only path dates from the 2012 import of `TrieMap` (2.10); 2.12 behaves the same.

Related: retronym/scala#677 (clone and `TrieMap.from` as snapshots) guards its new `snapshot()` calls with `nonReadOnly` because of this defect; with this change those guards are no longer needed, and a read-only map's clone could become a snapshot too. retronym/scala#673, retronym/scala#676 and retronym/scala#715 change other parts of `TrieMap`.

Found in the fourth wave of the coordinated code review of 2.13.x (finding WL1). Prepared with Claude.
