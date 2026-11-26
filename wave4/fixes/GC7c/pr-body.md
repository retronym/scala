`ArrayBuffer.flatMapInPlace` with a function that returns a view or iterator of the buffer empties the buffer or throws:

```scala
val a = ArrayBuffer(1, 2, 3); a.flatMapInPlace(_ => a.view.take(2))             // ArrayBuffer(), expected (1, 2, 1, 2, 1, 2)
val b = ArrayBuffer(1, 2); b.flatMapInPlace(_ => b)                              // ArrayBuffer(), expected (1, 2, 1, 2)
val q = ArrayDeque(1, 2); q.flatMapInPlace(x => q.iterator.map(_ * x))           // IndexOutOfBoundsException, expected (1, 2, 2, 4)
val r = ArrayDeque(1, 2, 3); r.flatMapInPlace(_ => r.view.take(2).to(Stream))   // IndexOutOfBoundsException, expected (1, 2, 1, 2, 1, 2)
```

`ListBuffer.flatMapInPlace` gives the expected results: it reads each result as the function returns it. `IndexedBuffer.flatMapInPlace`, which `ArrayBuffer`, `ArrayDeque`, `Queue` and `Stack` use, stores the results, clears the buffer and only then reads them, so a result that reads the buffer reads an empty one.

Each result that may see the buffer is now copied as the function returns it. Here the source is each result. A source is read as it is when it is strict and cannot see the receiver: an `Option`, an immutable collection other than a `LazyList` or `Stream`, a `mutable.ArraySeq` (which is what an `Array` argument arrives as), or an `ArrayBuffer`, `ArrayDeque`, `ListBuffer`, `UnrolledBuffer`, `mutable.HashSet` or `mutable.LinkedHashSet` other than the receiver. Anything else is copied into an `ArrayBuffer` first: every view and iterator, a `LazyList` or `Stream` (either may be built over the receiver and read lazily), a Java collection wrapper, and any other mutable collection.

Cost: a function returning an `Option`, a `List`, a `Vector`, an `Array` or another buffer allocates what it did before; `flatMapInPlace(x => if (p(x)) Some(x) else None)` takes no copy. A function returning a view or an iterator pays one copy per result: on JDK 21, `flatMapInPlace(_ => list.view)` over a three-element `ArrayBuffer` allocates 440 bytes per call, against 128 before. The alternative, collecting every result into one fresh buffer, would copy strict results too.

A decision for the owner: copying the source first gives the result of the same call with an equal, separate source. The alternative is to fail fast, as `ArrayBuffer`'s views already do: they throw `ConcurrentModificationException` when the buffer changes under them. Failing fast costs nothing for any source, but needs mutation tracking in the views and iterators of `ArrayDeque` and `UnrolledBuffer`, and turns the calls shown above into exceptions.

Long-standing since 2.13.0, not a regression. Related: retronym/scala#758 and retronym/scala#759 apply the same rule to the arguments of `addAll`, `insertAll`, `patchInPlace` and `subtractAll`, and retronym/scala#534 and retronym/scala#548 to `insertAll`. `mapInPlace` and `filterInPlace` with a function that reads the buffer are a different question and unchanged.

Part of scala/bug#12121.

Found in the fourth wave of the coordinated code review of 2.13.x (finding GC7). Prepared with Claude.
