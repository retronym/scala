`ArrayBuffer.flatMapInPlace` with a function that returns a view or iterator of the buffer empties the buffer or throws:

```scala
val a = ArrayBuffer(1, 2, 3); a.flatMapInPlace(_ => a.view.take(2))   // ArrayBuffer(), expected (1, 2, 1, 2, 1, 2)
val b = ArrayBuffer(1, 2); b.flatMapInPlace(_ => b)                    // ArrayBuffer(), expected (1, 2, 1, 2)
val q = ArrayDeque(1, 2); q.flatMapInPlace(x => q.iterator.map(_ * x)) // IndexOutOfBoundsException, expected (1, 2, 2, 4)
```

`ListBuffer.flatMapInPlace` gives the expected results: it reads each result as the function returns it. `IndexedBuffer.flatMapInPlace`, which `ArrayBuffer`, `ArrayDeque`, `Queue` and `Stack` use, stores the results, clears the buffer and only then reads them, so a result that reads the buffer reads an empty one.

Each result is now copied into an `ArrayBuffer` as it is returned unless it cannot read the buffer: an immutable collection other than a `LazyList`, a `mutable.ArraySeq`, or an `ArrayBuffer` or `ArrayDeque` other than the receiver. This is the rule the related pull requests for scala/bug#12121 apply to the arguments of `insertAll`, `addAll`, `patchInPlace` and `subtractAll`. A function returning a `List`, `Vector`, `Array` or another buffer costs what it did; one returning a view or an iterator pays one copy of each result. The alternative, collecting every result into one fresh buffer, would copy strict results too.

Long-standing since 2.13.0, not a regression. Related: retronym/scala#534, retronym/scala#548 and retronym/scala#538 (the same family for arguments); `mapInPlace` and `filterInPlace` with a function that reads the buffer are a different question and unchanged.

Part of scala/bug#12121.

Found in the fourth wave of the coordinated code review of 2.13.x (finding GC7). Prepared with Claude.
