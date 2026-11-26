This pull request holds a failing test and no fix: whether `updateWith` should support a remapping function that modifies the map, or reject it, is a design choice.

`mutable.HashMap.updateWith` and `mutable.LinkedHashMap.updateWith` give a wrong result when the remapping function modifies the same map. With `val m = mutable.HashMap(1 -> 2)`, `m.updateWith(1)(_ => { m.remove(1); Some(7) })` returns `Some(7)` and leaves `m` empty; `m.clear()` in place of `m.remove(1)` does the same. When the function removes the key and returns `None`, `mutable.Map(1 -> 1)` ends with `size == -1`, and in `HashMap(1 -> 1, 17 -> 17)` the entry for 17 is unlinked from iteration (size 0, empty `toList`) while `get(17)` still finds it. When the key is absent and the function adds enough other keys to grow the table, the new entry for 17 is linked into the bucket it had in the old table: it is in `toList` but `get(17)` and `contains(17)` miss it. A realistic trigger is a cache on a `LinkedHashMap` that evicts its eldest entries inside the function and evicts the key being updated. `mutable.Map(...)` is a `HashMap`, so the default map is affected.

The generic `mutable.Map.updateWith` (used by `TreeMap`, `LongMap`, `AnyRefMap`, `ListMap`, `OpenHashMap`, `CollisionProofHashMap`), `TrieMap`, and both overrides in 2.13.0 store the value in all these cases. The Scaladoc of `updateWith` does not forbid the function from touching the map. This is a regression in 2.13.1 from scala/scala#8073, which added the overrides to hash the key and walk the bucket once. They find the node (and its predecessor and bucket index) before calling the function and act on them afterwards, without checking that they are still current. scala/bug#12069 was an earlier defect in the same override.

`java.util.HashMap.compute`, `LinkedHashMap.compute` and `TreeMap.compute` throw `ConcurrentModificationException` for the same functions (their documentation says the function should not modify the map), and so does `updateWith` on a `java.util.HashMap` wrapped with `asScala`. The test therefore accepts either outcome: the map as the generic `updateWith` leaves it, or a `ConcurrentModificationException`.

Possible fixes:

- Support re-entrant modification, restoring 2.13.0 behaviour: after the function returns, check that the node is still in its bucket and that the table is the same one, and otherwise fall back to the generic `get`/`update`/`remove` path. Without a modification count this costs a second walk of the bucket on every call, which gives back part of what scala/scala#8073 gained.
- Reject it as Java does: add a modification count to `HashMap` and `LinkedHashMap` (a private field, binary compatible; both serialize through a proxy) and throw `ConcurrentModificationException` when it changed across the call. Programs that work today by accident (a function that adds other keys without moving the target bucket) would start to throw.

Either way the Scaladoc of `updateWith` should say what a function that modifies the map gets.

Found in the fourth wave of the coordinated code review of 2.13.x (finding GC6). Prepared with Claude.
