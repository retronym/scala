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
