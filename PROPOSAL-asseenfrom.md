# Fast path for `asSeenFrom(C.this, C)`

Status: draft for review, on `scala2-perf-integration` at `00e3bbafb8`. The instrumentation (`asf-instrument`) and the unsound ceiling variant (`asf-ceiling`) are local branches, not pushed.

## Problem

On the M4 (`00e3bbafb8`), `asSeenFrom` is 23% inclusive on the scala corpus but 7.6% on scalap. The hypothesis was that many calls pass a prefix/class pair under which the type maps to itself, yet still run the full `AsSeenFromMap`.

## Counts

One compile of each corpus with an instrumented `quick` build (`asf-instrument`, `-Dasf.stats`), JDK 25, no `-opt`. Each call is classified by how it returned: an existing early exit in `Types.asSeenFrom` (`isTrivial`, erased phase, `skipPrefixOf`), or the map with result `eq` input, `=:=` but not `eq`, or changed.

| | scala | scalap |
|---|---|---|
| calls | 11.91 M | 152 k |
| existing early exits | 7.60 M (63.8%) | 129.5 k (85.1%) |
| reach `AsSeenFromMap` | 4.31 M | 22.7 k |
| … result `eq` input | 2.47 M | 12.4 k |
| … `=:=` but not `eq` | 82 k | 2.1 k |
| … changed | 1.76 M | 8.2 k |
| map calls with `pre = clazz.this` | **2.01 M (46%)** | 7.1 k (31%) |

The scala corpus reaches the map 190 times as often as scalap, against about 80 times as many calls overall, and almost half of its map calls have one shape: `pre` is the `ThisType` of `clazz` itself. Breaking down the map calls on the scala corpus by prefix:

| prefix (scala corpus, map calls) | `eq` | `=:=`, not `eq` | changed |
|---|---|---|---|
| `clazz.this` | 1,982,493 | 25,510 | **0** |
| `D.this`, `D` a proper subclass of `clazz` | 123,236 | 7,942 | 1,131,581 |
| a `SingleType` other than below | 54,401 | 4,755 | 470,099 |
| `p.M.type`, `M.moduleClass == clazz` | 275,392 | 31,647 | 25,284 |
| others | ~37 k | ~11 k | ~133 k |

All 25,510 `clazz.this` calls that are not `eq` contain an `AnnotatedType`, and the map's only change is to drop `@uncheckedVariance` (`AsSeenFromMap` mixes in `KeepOnlyTypeConstraints`). No call with `pre = clazz.this` changed the type otherwise.

Where they come from (sampled stacks, 1 in 8): 93% are `TypeRef.relativize` and `seenFromOwnerInstantiated` in it, i.e. `TypeRef#baseType` of a class referred to through its natural prefix. In the compiler's cake (`Global.this.Symbol`, `Typers.this.Typer`) that prefix is the `this` of the enclosing class, which `skipPrefixOf` doesn't cover; for a top-level class the prefix is a package and the existing exit already applies. That explains the gap between the corpora.

Other shapes considered and not pursued:

- `D.this` seen from a base class `clazz`: the real work of `asSeenFrom` (90% changed).
- `p.M.type` seen from `M`'s module class: maps `M.this` to `p.M.type`, so 8% change; no cheap predicate tells them apart.
- Types that mention no class `this` and no class type parameter, but are not `isTrivial` because of a method type parameter: 368 k map calls, all `eq`. A guard would need a cached flag per type alongside `isTrivial` (more memory per `TypeRef`/`MethodType`). Future work if the first guard's gain warrants it.

## The guard

```scala
// Types.asSeenFrom
val trivial = this.isTrivial || phase.erasedTypes && ... || skipPrefixOf(pre, clazz) || isSeenFromOwnThis(pre, clazz)

private def isSeenFromOwnThis(pre: Type, clazz: Symbol) = pre match {
  case ThisType(sym) => (sym eq clazz) && isOwnThisSeenFromIdentity(clazz) && !containsAnnotationOrTypeVar(this)
  case _ => false
}
```

`isOwnThisSeenFromIdentity(C)` holds when, for `C` and each class outward along `owner` (until a package or term owner), `K.typeOfThis.baseType(K) eq K.tpe_*`. `ClassSymbol.selfTypeIsOwnType` answers that per class: `true` without work when the class has no self type (and isn't a module class), otherwise computed and cached per period and per `(rawInfo, thisSym.rawInfo)`; `false`, uncached, while either info is incomplete.

### Soundness

`AsSeenFromMap(C.this, C)` changes a type only at these nodes:

1. **`K.this`** (`thisTypeAsSeen`): walks `(pre, clazz)` from `(C.this, C)` to `((pre baseType clazz).prefix, clazz.owner)` until `clazz == K`, and returns `pre` there. With the self-type condition, `C.this.baseType(C)` is `C.tpe_*`, whose prefix is `C.owner.this`, so each step is `(K'.this, K')` and the match returns `K.this`. The walk stops at a package or term owner (`skipPrefixOf`), returning the node itself. The `pre` is stable, so `captureThis` never runs and `capturedParams` stays empty.
2. **A class type parameter `T` of `K`** (`classParameterAsSeen`): the same walk, then the argument of `pre.baseType(K)` for `T`. That base type is `K.tpe_*`, whose arguments are `K`'s type parameters, so `T` maps to `T.tpeHK` applied to the mapped arguments: the same unique `TypeRef`. `K.tpe_*` is not existential, so `capturedSkolems` stays empty. A type parameter of a base class of `K` that isn't on the chain never matches and is left alone.
3. **`SingleType`**: maps its prefix; unchanged if the prefix is.
4. **`AnnotatedType`**: drops non-`TypeConstraint` annotations and rewrites `This` trees in the kept ones' arguments. Excluded by the scan.
5. **`TypeVar`**: an instantiated one maps to its instance (`TypeVar.mapOver`). Excluded by the scan.

Everything else is `mapOver`, which returns the node itself when its parts are unchanged. With no captured params or skolems, `existentialAbstraction` and `deriveType` return their input.

Where the conditions fail, the counterexamples are real:

- `class SelfInt[T] { self: SelfInt[Int] => def f: T }`: `f.info.asSeenFrom(SelfInt.this, SelfInt)` is `=> Int`, because `typeOfThis.baseType(SelfInt)` is `SelfInt[Int]`. Found by the edge-case file. On the scala corpus 333 calls have a class with a refining self type on the chain (all happened to come out unchanged). The cake's self types (`self: Global =>`) don't refine their own class and pass: 1.27 M of the 2.01 M calls.
- `class Ann[+T] { def v: List[T @uncheckedVariance] }`: the map drops the annotation.

Residual differences, both believed unreachable in practice:

- `correspondingTypeArgument` finds the argument by name and tolerates a type parameter symbol that is not among the class's current type parameters (scala/bug#4365, logged as a dev warning). The guard would keep the stale symbol where the map substituted the current one.
- When `T.owner` has incomplete info, `isTypeParamOfEnclosingClass` treats it as enclosing and the map forces `baseType`. The guard forces nothing (it bails out if `C` or its self type is incomplete), so it can't introduce a cycle, but it can also skip a completion the map would have triggered.

### Verification

- Classfiles, including pickled signatures, byte-identical with the base for the whole scala corpus (5,549 classes), scalap and the edge-case file.
- `AsSeenFromTest` (junit): identity for nested cake-style classes with and without a non-refining self type; the self type, annotation and type variable counterexamples produce the map's result.
- `junit/testOnly scala.tools.nsc.typechecker.* scala.reflect.internal.*`: 287 passed.
- `partest --pos --neg`: 3,322 passed, 1 failed: `neg/t12494`, whose check file expects `[context] ++` debug lines that retronym/scala#148 (in the base) no longer prints because it creates contexts lazily. Not related to this change.

### Costs

- Four fields per `ClassSymbol` (two references, an int, a boolean) for the self-type cache.
- One `TypeCollector` walk per guarded call, in place of the map's walk (which does `baseType` / `baseTypeIndex` work at every `this` and class type parameter node). The `asf-ceiling` branch drops the walk (unsound) to price it; not measured, see below.

### Overlap with retronym/scala#199

#199 (devirtualising `Symbol` predicates) step 2 edits `AsSeenFromMap.apply`'s default case in `TypeMaps.scala` and makes `owner` / `typeOfThis` final in `Symbols.scala`. This change is kept as a separate, minimal diff on the integration tip, not rebased onto #199. The textual overlap is small: here, `TypeMaps.scala` gains a collector and `isOwnThisSeenFromIdentity` next to `skipPrefixOf`, and `Symbols.scala` gains the cache fields and `selfTypeIsOwnType` in `ClassSymbol`, which calls `typeOfThis` and reads `thissym`. They interact at run time: with the guard, `AsSeenFromMap.apply` runs about half as often on the scala corpus, so #199's gain at that call site shrinks if both land. Measure the second to land on top of the first.

## Measurements

All on the M4, patched JDK 26, `SCALA_RELEASE_OPT`, 8G heap. Dashboard: https://perflab.localhost/p/scala2-b7bb3d/ab/index.html

### JIT pilot: [`m4-asf-jit`](https://perflab.localhost/p/scala2-b7bb3d/ab/index.html#c-m4-asf-jit)

Scala corpus, one fork per variant, `jitRecording`.

| | base | proto | proto (A/A) |
|---|---|---|---|
| ms/op | 9,146 | 9,020 | 9,202 |
| alloc/op | 5,997 MB | 5,907 MB | 5,921 MB |
| C2 code bytes | 35.6 M | 36.4 M | 35.3 M |

The score gap (−1.4%) is smaller than the A/A spread (2.0%). The allocation drop (−1.3 to −1.5%, the `AsSeenFromMap` instances and their mapped copies) shows in both proto forks. `jit_diff` around `asSeenFrom` (`TypeRef.relativize`, `baseType`, `AsSeenFromMap.*`): every inlining flip between base and proto also appears, at a similar size, between proto and its A/A. The new `isSeenFromOwnThis` is a real call from `asSeenFrom` in both proto forks ("already compiled into a big method"), as is `AsSeenFromMap.<init>` in base.

The pilot showed no inlining shift, so the ceiling variant was not measured.

### Hardware counters: [`m4-asf-kperf`](https://perflab.localhost/p/scala2-b7bb3d/ab/index.html#c-m4-asf-kperf)

Scala corpus, one fork per variant, kperf cycles and instructions over the 4 measured ops, compiler thread only. Frames are inclusive.

| | instructions, base | proto | Δ | cycles, base | proto | Δ |
|---|---|---|---|---|---|---|
| compiler thread | 620.0 G | 598.8 G | −3.4% | 146.2 G | 142.5 G | −2.5% |
| `asSeenFrom` | 178.5 G | 158.3 G | −11.3% | 34.41 G | 31.54 G | −8.3% |
| `AsSeenFromMap` | 172.4 G | 148.8 G | −13.7% | 32.74 G | 29.15 G | −11.0% |
| `TypeRef.relativize` | 74.9 G | 58.4 G | −22.1% | 12.88 G | 11.02 G | −14.4% |
| `isSeenFromOwnThis` (new) | | 11.3 G | | | 1.92 G | |

Projected end to end, the net drop in `asSeenFrom` is **1.96% of compiler-thread cycles** and 3.25% of its instructions. The whole thread fell by more (2.53% of cycles), but the extra 0.83 G cycles is not attributed to the guard: two forks of the same base commit (this one and `m4-int00e3bb-kperf-l1d`) differ by 1.2 G cycles and 3.3 G instructions in total, and by 0.2 G cycles and 4.0 G instructions in the `asSeenFrom` frame. On that one pair of forks, the cycle drop in the frame is well above the noise, while the noise in the instruction drop is about 20% of its size.

Skipping 46% of the map calls removes only 11% of the map's cycles, so the skipped calls are much cheaper than the average map call. I haven't measured why.

The guard gives back 40% of its gross gain: 1.92 G of the 4.79 G cycles it saves. Of that, the annotation and type variable scan is 1.12 G, and the self-type check along the owner chain 0.63 G. A cheaper way to exclude annotations and type variables, for instance a cached per-type flag, would be worth at most about 0.8% of compiler-thread cycles (the scan's 1.12 G, less what the flag costs).

Other effects: allocation per op falls 1.9% (6,013 MB to 5,901 MB). Scores in the same forks are 9,495 and 9,266 ms/op, which is not evidence for a gain on its own at this sample size.

### Timing A/B: [`m4-asf-ab`](https://perflab.localhost/p/scala2-b7bb3d/ab/index.html#c-m4-asf-ab)

The frame-level projection fell just short of the 2% bar for an A/B, and Jason asked for one anyway. Planned for 10 rounds, stopped after 3 on scala to free the m4, because the rounds agree closely. No profiler, builds alternating fork by fork.

| scala corpus, ms/op | base | proto | Δ |
|---|---|---|---|
| round 1 | 9,141.8 | 9,045.6 | −1.05% |
| round 2 | 9,184.1 | 9,055.4 | −1.40% |
| round 3 | 9,145.7 | 9,020.9 | −1.36% |
| mean | 9,157 | 9,041 | **−1.27%** |

The standard deviation of the paired differences is 0.19 points, which gives a 95% interval of about ±0.5 points on 3 pairs. The ranges of the two builds do not overlap. Allocation falls 1.8% (5,985 MB to 5,879 MB per op).

Scalap, the control with few calls of this shape: 196.2 and 196.1 ms/op for base, 194.6 and 196.2 for proto, on 2 pairs. That is too few to resolve anything below about a point, but it shows no sign of a regression. Allocation is unchanged at 166 to 167 MB per op.

The gain is smaller than the kperf projection of 2.0 to 2.5%. That projection came from one pair of forks, whose whole-thread noise is 0.8% of cycles, so the two are not necessarily in conflict. I have not looked for another cause.

## Conclusion

The guard saves about 1.3% on the scala corpus and 1.8% of allocation, with byte-identical output. It costs four fields per `ClassSymbol` and 67 added lines with a subtle soundness argument. It helps code in the cake pattern and does little elsewhere.
