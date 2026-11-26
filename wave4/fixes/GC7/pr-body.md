`ArrayDeque.addAll`, `patchInPlace` and `subtractAll` read their argument while they change the deque, so an argument that reads the deque lazily, such as a view or an iterator of it, gives the wrong contents or throws:

```scala
val d = ArrayDeque(6, -2); d.addAll(d.view.reverse)         // ArrayDeque(6, -2, -2, -2), expected (6, -2, -2, 6)
val e = ArrayDeque(1, 2, 3); e.patchInPlace(1, e, 1)         // ArrayDeque(1, 1, 1, 3, 3), expected (1, 1, 2, 3, 3)
val f = ArrayDeque(1, 4); f.subtractAll(f.view.slice(0, 2)) // IndexOutOfBoundsException, expected ArrayDeque()
```

`Queue` and `Stack` inherit all three. `UnrolledBuffer.subtractAll` of a slice of itself leaves an element behind, and `ArrayBuffer` and `ListBuffer` throw `ConcurrentModificationException` for the same `patchInPlace` and `subtractAll` shapes. The expected results are what the same call gives with the argument taken from an equal, separate buffer, which is how `b ++= b` already behaves for every buffer.

`ArrayDeque.addAll` appends while it reads, and a reversed view indexes from the end that is growing. `IndexedBuffer.patchInPlace` overwrites slots with `update` before it has read the rest of the patch, then hands the remaining iterator to `insertAll`. `Shrinkable.subtractAll` removes one element at a time, and a buffer shifts the elements its argument is still reading. `ArrayDeque.prependAll` is unaffected, as it writes only slots before the start until it is done.

The three methods now copy the argument into an `ArrayBuffer` first unless it cannot read the receiver: an immutable collection other than a `LazyList`, a `mutable.ArraySeq` (which is what an `Array` argument arrives as), or an `ArrayBuffer` or `ArrayDeque` other than the receiver. That is the test retronym/scala#534 and retronym/scala#548 apply in `insertAll`, with `LazyList` added because a `LazyList` built from an iterator of the buffer reads it lazily. In `Shrinkable` the rule applies only when the receiver is a `Buffer`; sets and maps are the open part of scala/bug#12121 and get a separate pull request with failing tests. `subtractAll` cannot be overridden in `ArrayDeque` instead, as the override's `ArrayDeque` result type is a new method that MiMa reports as forward incompatible.

Cost: appending a view, an iterator or a mutable collection other than an `ArrayBuffer` or `ArrayDeque` to a non-empty `ArrayDeque` now copies it once, `Queue.enqueueAll(iterator)` included. An empty deque skips the copy, as every view or iterator of it is then empty, so building a deque costs what it did. `patchInPlace` copies such a patch once; `ArrayBuffer.insertAll` already copied an iterator for the part after the replaced slots. `subtractAll` on a buffer copies such an argument once, next to removals that are each linear in the buffer's length. Sources in the safe list cost what they did, and no result changes for a source that is not derived from the receiver.

Long-standing since 2.13.0, not a regression. This merges cleanly with retronym/scala#534 and retronym/scala#548, which cover `insertAll` and `prependAll` of `ArrayBuffer` and `ArrayDeque` and `patchInPlace` at index 0; the `UnrolledBuffer` cells and `ArrayBuffer.flatMapInPlace` are separate pull requests. Possible follow-up: once these land, one shared predicate in place of the match each site repeats; a helper needs a new class or member, which MiMa reports.

Part of scala/bug#12121.

Found in the fourth wave of the coordinated code review of 2.13.x (finding GC7). Prepared with Claude.
