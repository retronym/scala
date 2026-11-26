# Known defects not fixed on the integration build

Exclude these narrowly in a harness so they do not mask what is behind them.

| Id | Where | How to recognise it |
|---|---|---|
| LB2 | `mutable.TreeMap` and `mutable.TreeSet` range views (`range`, `rangeFrom`, `rangeTo`, `rangeUntil`) | `minAfter`, `maxBefore`, `foreachEntry`, steppers, `hashCode` and anything built on them answer from the whole tree; every property on such a view fails |
| GC7, GA1 | mutable collections given a view or iterator of themselves | fixed only on the wave-four integration build; do not generate such sources |
| GC1 to GC5 | `Range` and `NumericRange` at the Int and Long extremes | fixed only on the wave-four integration build |
| GC6 | `mutable.HashMap.updateWith` whose function modifies the map | open design question (retronym/scala#749) |
| GE4 | a partial function literal (`collect { case ... }`) in a method duplicated by specialization | scala/bug#8633, "found x1.type, required A1" |
| GE6 | `-opt:inline:**` on a method with about 190 or more exception handlers | `LimitExceededException: Too many allocated bytes` (retronym/scala#767) |
| GJ3 | a Java annotation argument that is a constant expression | the annotation is dropped from source |
| scala/bug#8156 | lambdas in constructors of specialized classes | different cause from GE7, still fails |
| J9 (wave one, remark) | a Java member whose name has a supplementary character (`int 𝑥`, `int x𝟙`), from source or class file | Scala source cannot select it: Scala encodes the surrogate pair as `$uD835$uDC65`, so "value 𝑥 is not a member" in both compilation orders (retronym/scala#291 leaves this open) |
| WX1 | structural call to a Unit method through a refinement member whose result type is `Any` | the call returns `null` instead of `()`; the fold harness generates no such refinement |
| GE6 (wider) | `-opt:local` alone, no inliner: a method with about 150 or more try/catch blocks | `CopyProp`'s `ProdConsAnalyzer`: 6 s at 100 handlers, `Too many allocated bytes` at 200 (2.13.18: 68 s, 25 GB allocated); in a harness, `-opt:local` compile timeouts |
| WP1 | `-Ycheck:all` on a guarded Int or String switch whose result is boxed | `Tree has erroneous type: default4()` at posterasure; the program is correct |
| LM7 | `Buffer.asJava` (`MutableBufferWrapper`) | `iterator().remove()` throws UnsupportedOperationException although `remove(int)` works; the open fix keeps the iterator read-only |
| WL1 | `TrieMap.readOnlySnapshot()` result | `snapshot()` or `readOnlySnapshot()` on it throws NullPointerException from `CAS_ROOT` |
| WL2 | `IntAccumulator`, `LongAccumulator`, `DoubleAccumulator`, `AnyAccumulator` | `apply(i)` / `update(i, x)` with `i == size` (or anywhere in the current chunk's spare capacity) return 0/null or write nothing instead of throwing |
| WL3 | `asJava` views of `BitSet`, `TreeSet[Int]`, `mutable.LongMap`, `IntMap`, `TreeMap[Int, V]` | `contains(null)`, `get(null)`, `remove(null)` act on key 0 |
| WP2 | `-Ycheck:all` on a structural call in a method of a generic value class | `Tree has erroneous type: s.get(...)` at pickler; the program is correct |
| WP3 | a try arm in a non-switch match that is the body of a partial function with a value class result | `VerifyError: Inconsistent stackmap frames` in `$anonfun...applyOrElse`; `-opt:local`: `AnalyzerException ... Incompatible stack heights` |
| WP4 | `-Ycheck:all` on a by-name argument reading a trait's lazy val of a type-parameter type, in a subclass | `Tree has erroneous type: C.this.lz()` at tailcalls; the program is correct |
| WP5 | a `return` evaluated with operands on the stack (in a method argument) inside a try/finally with another return | `Error while emitting ...: Index 0 out of bounds for length 0` (ASM frame merge); VerifyError on 2.13.18; `-opt:local`: `AnalyzerException` |
| WP6 | `-Ycheck:all` on a `return` inside a partial function literal | `Tree has erroneous type: <the return expression>` at typer to patmat; the program is correct |
| WP7 | an if or match whose branches construct a value class, one from a literal, used as a receiver | `FatalError: mergePrefixAndArgs(List(ErasedValueType(class V, Int), ErasedValueType(class V, Int(7))) ...): unsupported tps` in erasure |
