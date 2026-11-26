A structural call to a method returning `Unit` through a refinement member whose result type is `Any`, `AnyVal` or a type parameter returns `null`:

```scala
class K { def n: Unit = () }
(new K: { def n: Any }).n                // null, should be ()
def f[A](r: { def n: A }): A = r.n
f[Unit](new K)                          // null, should be ()
```

`K` conforms to `{ def n: Any }` because `Unit` conforms to `Any` (SLS 3.5.2), and `()` is the only value of `Unit`, so the call must yield `()`. The same member called through a trait that declares `def n: Any` does, because the bridge boxes the result. The value shows only where the caller keeps the result: it compares unequal to `()` and throws on `toString`.

CleanUp expands the call to `Method.invoke`, which returns `null` for a `void` method. `fixResult` substitutes `BoxedUnit.UNIT` only when the refinement member's own result type is `Unit`; when it erases to `Object` the result is passed through. Whether the selected method is `void` is known only at run time.

When the refinement member's result type (before erasure) is not a subtype of `AnyRef`, the call site now holds the `Method` it looked up in a local and, if `invoke` returned `null` and `getReturnType` is `void.class` (`BoxedUnit.TYPE`), yields `BoxedUnit.UNIT`. A `null` from a non-`void` method stays `null`. The cost, for result types `Any`, `AnyVal` and unbounded type parameters, is a null test per call and, for a null result, a `getReturnType` call and a reference comparison; no extra cache lookup. Calls whose result type is `AnyRef`, a class type or a type parameter bounded by `AnyRef` generate the same bytecode as before, since no `Unit` method can be selected through them. No library change, so no binary compatibility question.

This changes behaviour only for code that observed the `null`. It has been this way since at least 2.11; Scala 3's `reflectiveSelectable` returns `null` in the same situation, so the two would then differ until Scala 3 makes the same change. scala/bug#6199 is the runtime-reflection analogue, fixed in 2.10. Related structural-call fixes on the fork touch other parts of the expansion: retronym/scala#562 (argument evaluation order, composes with this one), retronym/scala#355, retronym/scala#394; retronym/scala#633 is another place where a `Unit` value surfaces as `null` (elements of `Array[Unit]`).

Found in the fourth wave of the coordinated code review of 2.13.x (finding WX1). Prepared with Claude.
