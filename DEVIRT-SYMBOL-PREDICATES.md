# Devirtualising Symbol's kind predicates and `info` / `rawInfo`

Status: SURVEY + PROTOTYPE, for review. Prototype on draft [retronym/scala#199](https://github.com/retronym/scala/pull/199) (branch `devirt-symbol-predicates`, on `scala2-perf-integration` at `00e3bbafb8`). The timing A/B waits for review.

## Problem

On the M4, the compiler thread runs at IPC ≈ 4.2 and is mostly instruction-bound. `vtable stub` + `itable stub` frames take **7.92% of compiler-thread cycles** (6.72% vtable, 1.20% itable) and 9.38% of its instructions, in the kperf profile of `00e3bbafb8` on the scala corpus (`m4-int00e3bb-kperf-l1d`, rendered with `jfrconv --hw cycles --lines` and with `--bci`, each stub attributed to the frame above it). The cost is spread over hundreds of call sites. The largest ones that call the methods in scope:

| caller (line / BCI) | callee | % of compiler-thread cycles |
|---|---|---|
| `FindMemberBase.walkBaseClasses:114` @106 | `Symbol.info` | 0.111 |
| `Symbol.info:1556` | `Symbol.rawInfo` | 0.193 |
| `TypeMaps.isPossiblePrefix:406` @1 / @8 | `isClass` / `isPackageClass` | 0.078 + 0.093 |
| `walkBaseClasses:141` | `isRefinementClass` | 0.138 |
| `walkBaseClasses:147` | `isClass` | 0.099 |
| `Symbol.isTopLevel:1049` @4 | `isPackageClass` | 0.053 |
| `Symbol.typeParams:1833` @9 | `rawInfo` | 0.065 |
| `Symbol.needsFlatClasses:845` | `rawowner.isPackageClass` (+ `isMethod`) | 0.045 |

Sites on the same lines that are *not* in scope, since the receiver is a `Type`: `walkBaseClasses:114` @109 `Type.decls` (0.097) and `Symbol.typeParams:1833` @12 `Type.typeParams` (0.123). The top three stub sites overall are also on `Type`/`TypeMap` and out of scope: `SubstMap.apply` → `Type.mapOver` (0.325), `TypeRef.mapOver` → `Function1.apply` from `mapConserve` (itable, 0.307) and `TypeRef.mapOver` → `TypeMap.apply` (0.193).

### Why these calls are megamorphic

HotSpot's receiver profile counts *receiver classes*, not target methods. `Symbol` has about 20 concrete subclasses. So a call site such as `currentBaseClass.isClass` sees more than two receiver classes and is compiled as a vtable call, even though `isClass` has only two implementations. CHA could bind a call statically only if the method had exactly one implementation among the *loaded* classes, and none of these does:

- `isClass`: `SymbolApi` (false) and `ClassSymbolApi` (true).
- `isPackageClass`: `Symbol`, `ClassSymbol` and `PackageClassSymbol`.
- `isRefinementClass`: `Symbol` and `RefinementClassSymbol`.
- `info` / `rawInfo`: `Symbol`, `NoSymbol` and `StubSymbol`, plus `SynchronizedSymbol` in runtime reflection. `NoSymbol` is always loaded. `StubSymbol`, `StubClassSymbol` and `StubTermSymbol` are loaded even by a trivial compile (`-Xlog:class+load` while compiling a one-line file with `build/quick`). So deleting one override and relying on CHA would not work. It would also be fragile, since loading the class that holds the other override deoptimises every dependent method.

The fix is one final implementation per method. The variants then have to be selected by data rather than by dispatch.

## Survey

### `isClass`

| class | override | can it be devirtualised? how |
|---|---|---|
| `api.SymbolApi` | `def isClass = false` | — |
| `api.ClassSymbolApi` | `final override def isClass = true` | removed (see API below) |
| `internal.Symbol` | inherited | **`final override def isClass = this.isInstanceOf[ClassSymbol]`** |

A type check against a non-final class costs one compressed-klass load, one load from the klass's primary-supers array at a constant depth, and a compare. It needs no field, has no indirect branch, and can be inlined into callers so that `isClass && !isPackageClass` folds together. Every `ClassSymbolApi` is an internal `ClassSymbol` (the API trait has a self-type of `ClassSymbol`), so the result is the same for every universe, including runtime reflection's `Synchronized*ClassSymbol` and the stub classes.

**API:** `ClassSymbolApi.isClass` (final, true) has to go, because `ClassSymbol` would otherwise inherit a final `Symbol.isClass` and a conflicting override. Binary compatibility: the internal `Symbol` keeps an `isClass` with the same signature. MiMa may report the trait's static `ClassSymbolApi.isClass$` as missing, which matters only for code outside scala-reflect that implements `ClassSymbolApi`, and nothing should. **TODO:** run `reflect/mimaReportBinaryIssues` and add a filter.

### `isPackageClass`

| class | override | can it be devirtualised? how |
|---|---|---|
| `api.SymbolApi` | abstract | — |
| `internal.Symbol` | `false` | **`final def isPackageClass = isClass && (rawflags & PACKAGE) != 0`** |
| `ClassSymbol` | `this hasFlag PACKAGE` | removed |
| `PackageClassSymbol` | `true` | removed |
| `Mirrors.RootClass`, `EmptyPackageClass` (extend `PackageClassSymbol`) | inherit `true` | `WellKnownSymbol` sets `PackageFlags \| STATIC`, so the bit is set |

Why it is equivalent: `newClassSymbol` creates a `PackageClassSymbol` exactly when `PACKAGE` is among the flags, and adds `PackageFlags`. `WellKnownSymbol` sets `PackageFlags` on the root and empty package classes. Nothing resets `PACKAGE`. The one `ClassSymbol` with `PACKAGE` that is not a `PackageClassSymbol` is an unpickler `StubClassSymbol` for a missing package (`UnPickler`: `stub.setFlag(PACKAGE | MODULE)`), and today it answers `true` through `ClassSymbol.isPackageClass`. The flag test keeps that, while an `isInstanceOf[PackageClassSymbol]` test would not. Reading `rawflags` instead of `hasFlag` is safe for two reasons. `PACKAGE` is in `InitialFlags` and is neither a late nor an anti flag, so `flags & PACKAGE == rawflags & PACKAGE` in every phase. And in runtime reflection, `PACKAGE` is outside `TopLevelPickledFlags`, so `SynchronizedSymbol.getFlag(PACKAGE)` never auto-initialises (its own comment says as much). A `ModuleSymbol` for a package also has `PACKAGE`, so the `isClass` test is needed.

### `isRefinementClass`

| class | override | can it be devirtualised? how |
|---|---|---|
| `Symbol` | `false` | **`final def isRefinementClass = this.isInstanceOf[RefinementClassSymbol]`** |
| `RefinementClassSymbol` | `true` | removed |

`RefinementClassSymbol` has no subclasses except runtime reflection's `RefinementClassSymbol with SynchronizedClassSymbol`, which the type check also accepts.

### `info` / `rawInfo`

| class | override | what it does | can it be devirtualised? how | risk |
|---|---|---|---|---|
| `Symbol` | — | `info`: complete while `_validTo == NoPeriod` (at most two completions), then `rawInfo`. `rawInfo`: select the history entry for the current phase and adapt or transform it if `_validTo < currentPeriod` | becomes the single final implementation | — |
| `NoSymbol` | `info = NoType`, `rawInfo = NoType` | NoSymbol's info never changes | **data:** `NoSymbol.info_=` (already an override) sets `validTo = Int.MaxValue` instead of `currentPeriod`. `_validTo != NoPeriod` skips completion, and `_validTo < curPeriod` is never true, so the shared `rawInfo` returns `noTypeHistory.info == NoType` without adapting or transforming it | something that reads `NoSymbol.validTo` sees `Int.MaxValue`. The users are `isInitialized` (still true), `Namers` (`runId(sym.validTo)`, only for symbols being completed) and `adaptInfos` (never reached) |
| `StubSymbol` (`StubClassSymbol`, `StubTermSymbol`, runtime's `SynchronizedStub*Symbol`) | `info = fail(NoType)`, `rawInfo = fail(NoType)` | report the missing class once (setting `IS_ERROR`), then return `NoType` | **cold path:** a stub's info is never complete (it has no info, and `StubSymbol.info_=` now resets `validTo` to `NoPeriod`). So the type check `isInstanceOf[StubSymbol]` goes on the `_validTo == NoPeriod` path of `info` (inside the completion loop) and `rawInfo` (before the `infos != null` assertion), which complete symbols never take | the error-reporting paths: neg tests for stubs (`t5148`, `t6440*`, `t7439`, `t8502*`, …). A `getClass eq` test would need four classes, two of them private to runtime reflection, so the cold path uses the interface check. A spare flag (bits 61–63 are free) would also work, but `cloneSymbol` copies flags onto a clone that is not a stub, and nothing is hot enough to need it |
| runtime `SynchronizedSymbol` | `info` / `rawInfo` = `gilSynchronizedIfNotThreadsafe { super.… }` (always takes the GIL, see the TODO there) | serialise completion and protect the racy reads of `infos` / `_validTo` | **constant per universe:** `final def info = if (isCompilerUniverseFinal) unsynchronizedInfo else synchronizedInfo`. `synchronizedInfo` is a virtual hook that does the same as `unsynchronizedInfo` by default, and `SynchronizedSymbol` overrides it to take the GIL. `isCompilerUniverseFinal` is a `final val` in `SymbolTable`, set from `isCompilerUniverse` | `ReflectGlobal` (ToolBox) mixes `runtime.SymbolTable` in last, so its `isCompilerUniverse` is false and its symbols lock as before. In a runtime universe, every symbol is a `SynchronizedSymbol` or `isAprioriThreadsafe` (`Symbol`'s constructor asserts it), and the second kind took no lock before either. If the val is read before it is initialised, it is false, and that path is identical for compiler symbols, so initialisation order cannot cause a problem. Runtime stubs now report while holding the GIL (it is reentrant) |

`Symbol.typeParams` has the same shape as `info` and is also overridden by `SynchronizedSymbol`. It is left for later (see Future work).

## Proposal

1. **Kind predicates** (`isClass`, `isRefinementClass`, `isPackageClass`) become final: type checks plus the raw `PACKAGE` bit. They need no new flags or fields. The only API change is dropping `ClassSymbolApi.isClass`.
2. **`info` / `rawInfo`** become final, one implementation each. NoSymbol is handled by data (a validity that never ends), stubs on the cold incomplete path, and runtime reflection by a per-universe constant that selects a virtual hook.

These could be two commits (1 is lower risk and independent). The prototype has them in one.

No new flags are needed. If a flag is ever wanted (a `STUB` bit, say), bits 61–63 are unused. 59 is `NEEDS_TREES` and 60 is `SCALA3X`. The table at the top of `Flags.scala` is out of date and omits 50 (`SYNTHESIZE_IMPL_IN_SUBCLASS`). Slots 51/53/54/55/57 hold late and anti flags that are marked unused, but `flags` folds them, so reusing them would need changes there.

### Alternatives considered

- **A per-class "kind" byte set in `Symbol`'s constructor** (extra constructor parameter, or a virtual consulted once). It is as cheap as a type check at the call site but costs a field, adds churn to every `Symbol` constructor, and would also have to handle `isPackageClass`'s stub case. Type checks need neither.
- **Leave the methods virtual and remove `NoSymbol`'s overrides, relying on CHA.** This fails, because stubs are loaded in every compile, and it would be fragile even if they were not.

## Evidence from the prototype

Campaign `m4-devirt-sym-prof` (M4, patched JDK 26 `jdk26-integration-branch-e9e67e`, `SCALA_RELEASE_OPT=1`, scala corpus single-shot 4+4, one fork per build, kperf cycles + instructions): https://perflab.localhost/p/scala2-b7bb3d/ab/index.html

Each stub is attributed to its caller frame. The table uses BCIs: the prototype shifts line numbers in `Symbols.scala`, so a line-level comparison of that file would be misleading. Figures are % of compiler-thread cycles.

| caller @BCI | callee | base | proto |
|---|---|---|---|
| `Symbol.info` @28 | `rawInfo` | 0.164 | 0 |
| `walkBaseClasses` @314 (line 141) | `isRefinementClass` | 0.149 | 0 |
| `walkBaseClasses` @106 (line 114) | `info` | 0.134 | 0 |
| `walkBaseClasses` @334 (line 147) | `isClass` | 0.084 | 0 |
| `Symbol.typeParams` @9 | `rawInfo` | 0.072 | 0 |
| `isPossiblePrefix` @8 / @1 | `isPackageClass` / `isClass` | 0.071 + 0.057 | 0 |
| `Symbol.isTopLevel` @4 | `isPackageClass` | 0.058 | 0 |
| `Symbol.needsFlatClasses` @31 | `isPackageClass` | 0.026 | 0 |
| `isOverridableMember` @1, `hasCompleteInfo` @11, `isStable` @1 | `isClass`, `rawInfo`, … | 0.027 + 0.022 + 0.026 | 0 + 0.003 + 0.003 |
| *(out of scope)* `walkBaseClasses` @109 | `Type.decls` | 0.103 | 0.028 |
| *(out of scope)* `Symbol.typeParams` @12 | `Type.typeParams` | 0.129 | 0.064 |
| *(out of scope)* `Symbol.isTopLevel` @1 | `owner` | 0.062 | 0.051 |

The new methods (`unsynchronizedInfo`, `unsynchronizedRawInfo`, the `synchronized*` hooks) have no stub samples.

Totals for the compiler thread, one fork per build, so this is not a timing result:

| | base | proto | Δ |
|---|---|---|---|
| stub share of cycles | 7.81% | 6.91% | −0.90 pt |
| stub share of instructions | 9.43% | 8.13% | −1.30 pt |
| cycles (4 measured ops) | 147.4 G | 141.6 G | −3.9% |
| instructions (4 measured ops) | 624.5 G | 586.5 G | −6.1% |

Removing the stubs alone accounts for about 0.9 pt of cycles, and the instruction count fell by more than that. This suggests that the inlined predicates fold into their callers (`isClass && !isPackageClass` becomes two type checks and a bit test). But one fork per build cannot separate that from noise. The timing A/B (base vs proto, several forks, scala and scalap as in `m4-int00e3bb-*`) waits for review.

## Testing

Run locally on `d2f3e64743`:

- partest on the stub tests (`neg/t5148`, `run/StubError*`, `run/t6440`, `t6440b`, `t7439`, `t8502`, `t8502b`, `t9268`, `t10171`, `typetags_without_scala_reflect_*`, `t6240-universe-code-gen`): 20/20.
- `partest --grep reflection`: 76/76. `--grep toolbox`: 154/154.
- `--grep runtime`: 673/674. The one failure is `bench/equality`, which fails to compile (`not found: value testing`) and is unrelated to this change.
- junit `scala.reflect.*` and `scala.tools.nsc.symtab.*`: 315/315.

Not yet run: the full partest suite, and `reflect/mimaReportBinaryIssues` (for `ClassSymbolApi.isClass`).

## Future work

- `Symbol.owner` (overridden by `NoSymbol`, which logs a warning) feeds `isTopLevel` and others. The same "data, not override" approach may apply.
- `isTerm` / `isType` / `isMethod` / `isModule` have the same API shape as `isClass` (`…SymbolApi` final overrides) and could become type checks or flag tests.
- `Symbol.typeParams`: `SynchronizedSymbol` overrides it, and the same hook pattern would apply.
- The largest remaining stub sites are on `Type`/`TypeMap` (`mapOver`, `apply`, `decls`, `typeParams`, `baseType`, `underlying`). These are polymorphic by design and need a different approach.
