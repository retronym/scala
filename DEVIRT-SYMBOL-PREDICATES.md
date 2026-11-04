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

**Timing A/B, step 1 vs base** (campaign `m4-devirt-sym-ab`, M4, no profiler, builds alternating by fork; cancelled after 4 of 5 rounds): scala corpus 9,232 → 9,054 ms/op (**−1.9%** ±6.3 at 99.9%), scalap 196.2 → 193.5 ms/op (**−1.4%** ±2.7). Both corpora move in the same direction, but with 4 forks neither delta clears perflab's 99.9% interval.

### Step 2: `owner`, `thisSym`, `typeOfThis`, `tpe_*`, `mapOver`, `decls`

Commit `85df493655` on the same branch: final `owner` / `thisSym` / `typeOfThis` / `tpe_*` with the overrides folded in (module classes' cached self type behind a virtual `customTypeOfThis`, runtime reflection's `TypeSymbol` locks behind the per-universe constant), `TypeMap.mapOverType` (a `TypeRef` check, then the identity cases `ThisType`/`NoPrefix`, before the virtual call) used by `TypeMap.mapOver`, `SubstMap.apply` and `AsSeenFromMap.apply`, and a `ClassInfoType` check before `decls` in `walkBaseClasses`.

Profile campaign `m4-devirt-sym-prof2` (same settings, step 1 `d2f3e64743` vs step 2, one fork each), stub cycles by caller @BCI, % of compiler-thread cycles:

| caller @BCI | step 1 | step 2 |
|---|---|---|
| `SubstMap.apply` @18 (`mapOver`) | 0.314 | 0 |
| `AsSeenFromMap.apply` @86 (`mapOver`) | 0.147 | 0 |
| `TypeMap.mapOver` @8 | 0.092 | 0 |
| `ThisType.underlying` @4 (`typeOfThis`) | 0.066 | 0 |
| `AsSeenFromMap.loop$4` @60 (`owner`) | 0.064 | 0 |
| `Symbol.isTopLevel` @1 (`owner`) | 0.057 | 0 |
| `Symbol.typeOfThis` @4 (`tpe_*`) | 0.049 | 0 |
| `walkBaseClasses` @109 (`decls`) | 0.016 | 0 |
| new slow paths: `mapOverType` @38, `typeOfThis` @8, `walkBaseClasses` @281 | — | 0.034 + 0.020 + 0.014 |
| `TypeRef.mapOver` @443 (`mapConserve`'s `Function1.apply`, itable) | 0.288 | 0.365 |
| `AsSeenFromMap.isTypeParamOfEnclosingClass` @1 (`isTypeParameter`) | 0 (inlined into its caller) | 0.063 (now a compiled frame `_[j]`) |

| compiler thread | step 1 | step 2 |
|---|---|---|
| stub share of cycles | 6.95% | 6.12% |
| stub share of instructions | 8.20% | 6.90% |
| cycles (4 measured ops) | 139.7 G | 140.4 G (+0.5%) |
| instructions (4 measured ops) | 585.0 G | 585.0 G (0.0%) |

The targeted stubs are gone, but the total does not move.

**JIT pilot** (`m4-devirt-sym-jit`: `jitRecording` over each whole fork, one fork each of step 1, step 2 and step 2 again as an A/A, read with `perflab api jit` / `jit_diff`):

| | step 1 | step 2 | step 2 (A/A) |
|---|---|---|---|
| C2 code bytes | 35.37 MB | 36.41 MB | 36.52 MB |
| C2 inlining failures | 30,132 | 32,036 | 32,341 |
| `Symbol.owner()` refused as "too big" (in the 16 largest compiled methods) | 0 | 14 | 16 |

The A/A agrees with step 2, so this is not profile pollution. Step 2 made `owner` final with the overrides folded in, which grew it from a 5-byte accessor to 55 bytes of bytecode. That is over C2's `MaxInlineSize` (35), the limit at call sites C2 doesn't consider hot. `AsSeenFromMap.isTypeParamOfEnclosingClass`, for example, inlined `owner` in step 1 and calls it in step 2, which matches its new `_[j]` frame in the profile. Other changes: `TypeRef.mapOver` is a direct call from `mapOverType` but too big to inline there ("hot method too big"), and `ThisType.underlying` grew from 296 to 2,720 bytes of code, because it now inlines the whole `typeOfThis` → `tpe_*` → `info` chain.

**Fix** (`8d581317e6`): `owner`'s fast path returns `rawowner` unless the phase flattens classes or the symbol is NoSymbol. The rest goes to `ownerSlow`, and `owner` is now 31 bytes. The other new finals are under the limit (`typeOfThis` 20, `tpe_*` 20, `thisSym` 17, `isPackageClass` 26, `rawInfo` 20).

Profile with `jitRecording` (`m4-devirt-sym-prof3`: kperf cycles + instructions, step 2 `85df493655` vs fix `8d581317e6`, one fork each):

| | step 2 | fix |
|---|---|---|
| `owner()` refused "too big" (16 largest methods) | 15 | 0 |
| C2 code bytes | 54.29 MB | 52.86 MB (−2.6%) |
| C2 inlining failures | 31,956 | 31,060 (−2.8%) |
| compiler-thread cycles | 140.1 G | 138.4 G (−1.2%) |
| compiler-thread instructions | 579.6 G | 575.8 G (−0.7%) |
| stub share of cycles | 6.13% | 6.18% |

C2 code bytes are higher here than in the pilot because the profiler is loaded, so compare only within one campaign. `isTypeParamOfEnclosingClass` is inlined again (`_[i]`), but its call to `isTypeParameter` still goes through a stub (0.11%), so making `isTypeParameter` final is the next step. Specialising `mapConserve` for `TypeMap` comes after that (`TypeRef.mapOver` @443, 0.31–0.34%).

## Testing

Run locally on `d2f3e64743`:

- partest on the stub tests (`neg/t5148`, `run/StubError*`, `run/t6440`, `t6440b`, `t7439`, `t8502`, `t8502b`, `t9268`, `t10171`, `typetags_without_scala_reflect_*`, `t6240-universe-code-gen`): 20/20.
- `partest --grep reflection`: 76/76. `--grep toolbox`: 154/154.
- `--grep runtime`: 673/674. The one failure is `bench/equality`, which fails to compile (`not found: value testing`) and is unrelated to this change.
- junit `scala.reflect.*` and `scala.tools.nsc.symtab.*`: 315/315.

Step 2 (`85df493655`), the `owner` fix (`8d581317e6`) and the Type-hierarchy prototype (`8eeb7c633c`) each pass: junit `scala.reflect.*`, `nsc.symtab.*` and `nsc.typechecker.*` 333/333, the stub tests, `--grep reflection` 76/76, `--grep toolbox` 154/154, and full `pos` + `neg` 3322/3325.

**`neg/t12494` is not a regression from this PR.** It also fails on the base `00e3bbafb8` (`partest test/files/neg/t12494.scala` in the `perf-rebase` checkout, built at that commit), with the same diff. Its check file records `-Ylog:superaccessors -Ydebug` output, and the `[log superaccessors] [context] ++ …` lines are missing in both builds. That fits retronym/scala#148 (typing transformers' contexts made lazily), which is in the base: contexts that are never created are never logged. The check file needs updating on the integration branch.

Not yet run: `partest --run --jvm` for step 2, and `reflect/mimaReportBinaryIssues` (for `ClassSymbolApi.isClass`).

## Remaining stub sites: receiver histograms

The prototype leaves the largest stub sites in place, on `Type`, `TypeMap` and a few more `Symbol` methods. Most of them have the same shape as `isClass`. Many receiver *classes* reach one to three *implementations*, and C2's profile counts classes: there are 31 classes at `SubstMap.apply → mapOver`, for example, but two implementations take 94% of the calls. A final entry point with a fast path for the dominant case, which calls a virtual on the slow path, would turn most of these calls into a type check and a direct or inlined call.

**Data.** "Stub %" is the share of compiler-thread cycles in that site's vtable stub, from the base kperf profile `m4-int00e3bb-kperf-l1d` (`--bci`). The calls, classes and implementations are exact counts from a caller audit of the base build on the scala corpus, run locally (`perflab run --local --profile audit:audit/receivers.audit`, out `labs/scala2/out/runs/20261103-225136-audit-audit_receivers.audit`). The spec has one `sites … from=<caller> depth=0 qualify=class` probe per class that declares the callee, in reflect and nsc, 185 probes, generated from `javap`. The probes walk the stack on every hit, so the measured op ran into JMH's timeout. The counts cover the setup compile plus part of the measured one, about one compile in all; the shares are what matter.

| call site (stub %) | calls | receiver classes → implementations | top implementations (share of calls) | fast path |
|---|---|---|---|---|
| `SubstMap.apply` → `Type.mapOver` (0.300) | 47.3 M | 31 → 15 | `Type` (identity: `UniqueThisType` 28%, `NoPrefix` 25%) 54%, `TypeRef` 40% | in `SubstMap.apply` (or a final `TypeMap.mapOverFast`): `tp match { case tr: TypeRef => tr.mapOver(this)` (one implementation for every TypeRef class, so CHA binds it) `case _: ThisType \| NoPrefix => tp` `case _ => tp.mapOver(this) }`. Covers 94% |
| `AsSeenFromMap.apply` → `mapOver` (0.109) | 15.5 M | 23 → 10 | `TypeRef` 64%, `Type` 19%, `MethodType` 6% | same helper. Covers 83% |
| `TypeMap.mapOver(List)` → `mapOver` (0.064) | 2.0 M | 27 → 11 | `TypeRef` 48%, `Type` 42%, `SingleType` 9% | same helper |
| `Symbol.typeParams` @12 → `Type.typeParams` (0.129) | 29.4 M | 18 → 8 | `Type` (Nil: `ClassInfoType` 66%, `TypeBounds` 6%) 72%, `PolyType` 22% | in `Symbol.typeParams`: `rawInfo match { case pt: PolyType => pt.typeParams` `case _: ClassInfoType => Nil` `case tp => tp.typeParams }`. Covers 94% |
| `walkBaseClasses` @109 → `Type.decls` (0.103) | 25.2 M | 4 → 3 | `ClassInfoType` 80% (incl. `PackageClassInfoType` 2%), `PolyType` 20% | at the site: `info match { case ci: ClassInfoType => ci.decls case tp => tp.decls }`. Covers 80%. If the `PolyType` case unwraps its result too, 99.7% |
| `ThisType.underlying` → `Symbol.typeOfThis` (0.094) | 14.4 M | 6 → 3 | `Symbol` 87% (`ClassSymbol` 79%, `RefinementClassSymbol` 8%), `ModuleClassSymbol` 12% (with `PackageClassSymbol`) | final `typeOfThis = if (isInstanceOf[ModuleClassSymbol]) moduleTypeOfThis else thisSym.tpe_*`. `RootClass`'s override extends `ModuleClassSymbol`, so it stays on the virtual slow path |
| `Symbol.typeOfThis` → `tpe_*` (0.057) | 12.7 M | 6 → 2 | `TypeSymbol` 60%, `Symbol` 40% (`TermSymbol`: self-type symbols) | final `tpe_* = if (isInstanceOf[TypeSymbol]) typeSymbolTpe else symbolTpe`. Runtime reflection's `SynchronizedTypeSymbol.tpe_*` lock goes behind the same per-universe hook as `info` |
| `AsSeenFromMap.isTypeParamOfEnclosingClass` → `isTypeParameter` (0.090) | 12.4 M | 9 → **1** | `TypeSymbol` 100% | final `isTypeParameter = isInstanceOf[TypeSymbol] && isTypeParameterOrSkolem && !isSkolem`, as for `isClass` |
| `AsSeenFromMap.loop$4` → `owner` (0.043) | 6.2 M | 4 → **1** | `ClassSymbol.owner` 100% | final `owner` (below) |
| `Symbol.isTopLevel` @1 → `owner` (0.062; still 0.051 in the prototype) | 6.1 M | 16 → 5 | `Symbol` 56%, `ClassSymbol` 37%, `ModuleSymbol` 7% | final `owner = if (needsFlatClasses && (isClass \|\| isModule)) rawowner.owner else { if (this eq NoSymbol) warnNoSymbolOwner(); rawowner }`. `ClassSymbol`'s and `ModuleSymbol`'s overrides already have that form (`needsFlatClasses` includes `!isMethod`), and `RootSymbol`'s `owner = rootOwner` equals its `rawowner` |
| `Type.dealiasWiden` → `widen` (0.043) | 9.7 M | 28 → 4 | `Type` (identity) 97% | in `dealiasWiden`, or a final entry: `if (isInstanceOf[TypeRef]) this else widen` (`TypeRef` doesn't override `widen`; `TypeRef` classes are 88% of calls) |
| `SimpleTypeProxy.baseType` → `baseType` (0.056) | 9.3 M | 15 → 5 | `TypeRef` 92% | `underlying match { case tr: TypeRef => tr.baseType(clazz) case tp => tp.baseType(clazz) }`. `TypeRef.baseType` has one implementation |
| `TypeRef.baseType` @29 → `sym.info.baseType` (0.054) | 8.1 M | 3 → 2 | `CompoundType` 72% (`ClassInfoType` 63%, `RefinedType0` 9%), `PolyType` 28% | at the site: `case ci: ClassInfoType` first. Only three receiver classes, so a larger `TypeProfileWidth` might let C2 do it unaided (see below) |
| `Type.asSeenFrom` @180 → `isTrivial` (0.050) | 8.3 M | 23 → 11 | `TypeRef` 63%, `MethodType` 18%, `NullaryMethodType` 9% | `TypeRef` check first. Covers 63% |

Together these sites take about 1.4% of compiler-thread cycles in stubs, before counting any gain from inlining.

**Not covered by the audit.** The two visitor sites in `TypeRef.mapOver` call the *map*, not the type: @443, `mapConserve`'s `Function1.apply` (itable, 0.303), and @5, `TypeMap.apply` (0.213). The audit didn't count them. Time-weighted callee frames in the profile put `AsSeenFromMap.apply` at 53% and 72%, then `SubstSymMap` and `SubstMap`. A type check on `AsSeenFromMap` (if it is effectively final) before the virtual call would cover most of it. The `mapConserve` site would need a specialised `mapConserve(TypeMap)` loop that calls `TypeMap.apply` directly instead of through `Function1`.

**A JVM-flag experiment.** Several sites (`TypeRef.baseType`, `walkBaseClasses` → `decls`) have only three or four receiver classes. C2 inlines at most two by profile (bimorphic). A campaign variant with `-XX:TypeProfileWidth=4` (and/or `TypeProfileMajorReceiverPercent`) would show how much of this C2 recovers by itself. That is a cheap check before writing fast paths by hand.

## Type hierarchy: proposal

The sites the parent session named, as they stand after step 2 and the `owner` fix. Stub % is the share of compiler-thread cycles in the latest profile (`m4-devirt-sym-prof3`, fix cell), against base `00e3bbafb8` (`m4-int00e3bb-kperf-l1d`, by BCI):

| site | base | now | status |
|---|---|---|---|
| `SubstMap.apply:755` → `Type.mapOver` | 0.300 | 0 | done in step 2 (`mapOverType`). Its slow path `mapOverType` @38 is 0.048, by time `MethodType` 43%, `PolyType` 42% |
| `Symbol.typeParams:1833` @9 → `rawInfo` | 0.072 | 0 | done in step 1 |
| `Symbol.typeParams:1833` @12 → `Type.typeParams` | 0.129 | 0.064 | **proposal B** |
| `TypeRef.mapOver:2410` @5 → `TypeMap.apply` (`map(pre)`) | 0.213 | 0.249 | **proposal A** |
| `TypeRef.mapOver:2419` @443 → `Function1.apply` (itable, `args mapConserve map`, `mapConserve` is `@inline`) | 0.303 | 0.314 | **proposal A** |
| `AsSeenFromMap.isTypeParamOfEnclosingClass` → `isTypeParameter` | 0.090 | 0.113 | **proposal C** |

**Receivers at the visitor sites.** A caller audit of the base build (`audit/typemap-receivers.audit`, run `out/runs/20261104-000150-audit-…`, about two scala-corpus compiles) probes each `TypeMap` class's `apply(Type)`, and its `apply(Object)` bridge, filtered by the caller `TypeRef.mapOver`:

| site | calls | receiver classes | top classes |
|---|---|---|---|
| @5 `map(pre)` | 121.7 M | 36 | `SubstTypeMap` 29%, `SubstSymMap` 27%, `AsSeenFromMap` 23% (79% together) |
| @443 `mapConserve` → `Function1.apply` | 61.6 M | 34 | `SubstTypeMap` 39%, `AsSeenFromMap` 29%, `SubstSymMap` 26% (94% together) |

By time (callee frames in the profile), `AsSeenFromMap.apply` takes 49–70%, since it does the most work per call. These three classes have three `apply` implementations:
- `AsSeenFromMap.apply`, a class with no subclasses;
- `SubstMap.apply`, inherited by `SubstTypeMap`;
- `SubstSymMap.apply`, which overrides it. `SpecializeTypes` subclasses it anonymously, but the subclass doesn't override `apply`.

### A. A final `TypeMap` dispatcher for `TypeRef.mapOver`'s visitor calls

```scala
// in TypeMap
final def applyFast(tp: Type): Type =
  if (this.isInstanceOf[AsSeenFromMap]) this.asInstanceOf[AsSeenFromMap].apply(tp)   // one implementation: binds statically
  else if (this.isInstanceOf[SubstMap[_]]) this.asInstanceOf[SubstMap[_]].apply(tp) // 2 implementations, ~2 classes: bimorphic
  else apply(tp)
final def mapConserveTypes(tps: List[Type]): List[Type]  // mapConserve's loop, calling applyFast
```

`TypeRef.mapOver` would use `map.applyFast(pre)` and `map.mapConserveTypes(args)`. The second also removes the `Function1` itable and the `apply(Object)` bridge, so 79–94% of these calls become a type check and a direct call. The 0.56% of cycles in the two stubs is the ceiling, plus whatever inlining follows. The risk is size: `applyFast` must stay small enough to inline (under 35 bytes, see the `owner` lesson), and `AsSeenFromMap.apply` / `SubstMap.apply` must stay out-of-line calls, not be inlined into `TypeRef.mapOver`. The JIT pilot will check both. Other callers with the same shape, `TypeMap.mapOver(List)`, `mapOverArgs` and `MethodType.mapOver`'s params, can use the same helper later.

### B. A fast path for `rawInfo.typeParams` in `Symbol.typeParams`

The audit counts at @12 are `ClassInfoType` 66% (`Type.typeParams = Nil`), `PolyType` 22%, `TypeBounds` 6% (`Nil`) and `ClassNoArgsTypeRef` 3%:

```scala
rawInfo match { case _: ClassInfoType => Nil  case pt: PolyType => pt.typeParams  case tp => tp.typeParams }
```

This covers 88%. `PolyType.typeParams` is a case accessor that no subclass overrides, so it binds statically. `ClassInfoType` may be subclassed (`PackageClassInfoType`), but neither class overrides `typeParams`.

### C. A final `isTypeParameter`

One implementation is reached from 9 receiver classes (`TypeSymbol.isTypeParameter = isTypeParameterOrSkolem && !isSkolem`). Make it `final def isTypeParameter = this.isInstanceOf[TypeSymbol] && isTypeParameterOrSkolem && !isSkolem`, in `Symbol`, as for `isClass`. `isTypeParameterOrSkolem` and `isSkolem` are virtual too. `TypeSkolem` overrides `isSkolem` (true), and the rest is flags, so check that it stays under 35 bytes, with `TypeSkolem` as a type check.

### D. Smaller ones, same pattern (each 0.04–0.06%)

`copyTypeRef` → `isAliasType` (another kind predicate: `isInstanceOf[AliasTypeSymbol]`), `SimpleTypeProxy.baseType` → `TypeRef.baseType` (98%), `TypeRef.relativize` → `isTrivial` (`TypeRef` 79%), `normalizePlus` → `TypeRef.normalize` (95%), `AsSeenFromMap.loop$4` → `prefix` (`TypeRef` 83%), `SimpleTypeProxy.baseType` → `ThisType.underlying` (91%), and `mapOverType`'s slow path (add `MethodType`/`PolyType` cases).

**Plan.** Prototype A, B and C as one commit (A is the largest). Run tests locally, then on the m4: a kperf profile with `jitRecording` of the current tip against the prototype (2 cells × 1 fork, about 15 min), asking the parent session for m4 time first. D waits for review.

### Prototype results

`m4-devirt-type-prof`: the `owner` fix against A, B and C on top of it, m4, scala corpus, one fork per build, with `jitRecording`. This is a profile, not a timing A/B, so the differences in totals are indicative only. Shares are of compiler-thread samples, by BCI.

| | `owner` fix | prototype | change |
|---|---|---|---|
| compiler-thread cycles | 139.97 G | 137.29 G | −1.9% |
| compiler-thread instructions | 577.3 G | 569.0 G | −1.4% |
| stubs, share of cycles | 6.01% | 5.80% | −0.21 |
| itable stubs, share of cycles | 1.24% | 0.95% | −0.29 |
| vtable stubs, share of cycles | 4.77% | 4.84% | +0.07 |
| stubs, share of instructions | 6.97% | 6.74% | −0.23 |

Stub attribution at the sites the prototype changed:

| site | cycles before → after | instructions before → after |
|---|---|---|
| `TypeRef.mapOver` @443 → `Function1.apply` (itable) | 0.300 → 0 | 0.494 → 0 |
| `TypeRef.mapOver` @5 → `TypeMap.apply` | 0.240 → 0 | 0.308 → 0 |
| `TypeRef.mapOver` `coevolveSym` call (was @700, now @124) | 0.030 → 0.030 | 0.032 → 0.040 |
| `Symbol.typeParams` @12 → `Type.typeParams` (now @40, the slow path) | 0.077 → 0.013 | 0.101 → 0.017 |
| `isTypeParamOfEnclosingClass` → `isTypeParameter` | 0.069 → 0 | 0.131 → 0 |
| new: `applyFast1` @12 → `SubstMap.apply` | 0 → 0.237 | 0 → 0.411 |
| new: `applyFast1` @18 → `TypeMap.apply` (slow path) | 0 → 0.084 | 0 → 0.108 |
| total | 0.716 → 0.364 | 1.066 → 0.576 |

B and C did what they were meant to. In A, the `AsSeenFromMap` branch and the removal of the `Function1` itable worked: `applyFast` inlines into `TypeRef.mapOver`, and `AsSeenFromMap.apply` is a direct call ("already compiled into a medium method"). The `SubstMap` branch did not. The call at `applyFast1` @12 is still a vtable call and is now the largest stub site in the profile. By time its callees are `SubstSymMap.apply` 57% and `SubstMap.apply` 43%.

The proposal assumed C2 would make that call bimorphic. The recording shows it tried and gave up: `applyFast1` has 5 deopts, all `bimorphic_or_optimized_type_check`, and its last C2 compile inlines nothing. Those two implementations are reached from several classes (`SubstTypeMap` and its subclasses, `SubstSymMap` and its anonymous subclass), and `applyFast1` is a single method whose one receiver profile is shared by every caller.

`jit_diff`:
- `TypeRef.mapOver`'s last C2 compile went from 14.6 KB with 1407 inlined bytes to 3.4 KB with 113. Besides the helpers moving out, `copyTypeRef` is no longer inlined there ("already compiled into a big method"). I don't know whether that part is the change or compile-order noise, and the campaign has no A/A to tell.
- `mapConserveTypes` isn't inlined into `TypeRef.mapOver` ("already compiled into a big method", 4.0 KB). That is expected for a loop, and it adds a call that was not there when `mapConserve` was inlined.
- `isTypeParameter` now compiles at C2, and `Symbol.typeParams` grew slightly, 2304 → 2536 bytes.
- Whole-program totals: C2 code +1.1% (53.9 → 54.5 MB), inlining failures +2.3%, deopts −2.8%. The largest per-method differences (±50–175 KB, in methods the prototype doesn't touch, such as `Traverser.traverseTreess`) look like inlining-budget noise between forks.

**Next step, if wanted.** Bind both `SubstMap` branches statically, instead of relying on the profile. No subclass of `SubstSymMap` overrides `apply`, so a type check on it binds by CHA. `SubstMap.apply` can't bind because `SubstSymMap` overrides it, but its body can move to a final method:

```scala
private final def applyFast1(tp: Type): Type =
  if (this.isInstanceOf[SubstSymMap]) this.asInstanceOf[SubstSymMap].apply(tp) else applyFast2(tp)
private final def applyFast2(tp: Type): Type =
  if (this.isInstanceOf[SubstMap[_]]) this.asInstanceOf[SubstMap[_]].substMapApply(tp) else apply(tp)
```

The ceiling is the 0.237% of cycles and 0.411% of instructions at @12. The cost is one more type check on the path of the other 30-odd map classes. I would make this change and profile it before spending m4 time on a timing A/B of the Type work.

### Static `SubstMap` branches: results

`m4-devirt-subst-prof`: the previous prototype (`types`) against the change above (`subst`), m4, scala corpus, one fork per build, with `jitRecording`. Each helper is 22 bytes.

| site | cycles before → after | instructions before → after |
|---|---|---|
| `applyFast1` @12 → `SubstMap.apply` | 0.197 → 0 | 0.348 → 0 |
| slow path → `TypeMap.apply` (`applyFast1` @18, now `applyFast2` @18) | 0.191 → 0.115 | 0.227 → 0.172 |

The `SubstMap` dispatch stub is gone. What remains at `applyFast2` @18 is the generic maps the fast path doesn't cover, by time `UnCurry$$anon$1` and `PostErasure.elimErasedValueType`, then `LambdaLift$$anon$1` and `TypeTraverser`.

| | `types` | `subst` | change |
|---|---|---|---|
| compiler-thread cycles | 139.22 G | 137.20 G | −1.45% |
| compiler-thread instructions | 580.9 G | 573.3 G | −1.29% |
| stubs, share of cycles | 5.56% | 5.52% | −0.04 |
| stubs, share of instructions | 6.41% | 6.30% | −0.11 |
| C2 code | 53.37 MB | 53.08 MB | −0.6% |

**These totals don't show a gain, here or in the previous section.** The `types` code is the same in `m4-devirt-type-prof` and this campaign, and its one fork differs between them by 1.4% in cycles (137.29 G, 139.22 G), 2.1% in instructions (569.0 G, 580.9 G) and 0.24 points of stub share (5.80%, 5.56%). That is as large as the differences in both tables of totals. The site-level attribution is what these profiles can support; whether the Type work saves time needs a timing A/B.

`jit` for these methods:
- `substMapApply` and `SubstSymMap.apply` are called directly and aren't inlined at `applyFast` ("hot method too big", "too big"), as intended.
- C2 now also compiles `applyFast` as a large standalone unit, 26.8 KB with `AsSeenFromMap.apply` inlined, up from 704 bytes. In `TypeRef.mapOver`'s last compile, `applyFast` is therefore a real call ("already compiled into a big method"), where before it was inlined, and `mapConserveTypes` is now inlined into `TypeRef.mapOver`. A direct call is cheaper than the stub it replaced, but this is the size risk section A named.
- `applyFast1` and `applyFast2` have 8 and 5 `class_check` deopts, where `applyFast1` previously had 5 `bimorphic_or_optimized_type_check`. Each is a small number against about 3700 in the fork.
- In this campaign's `types` fork, C2 inlined `SubstSymMap.apply` into `applyFast1`, which it didn't do in the previous campaign's fork of the same code. That moved the stub cost between @12 and @18 (0.237/0.084 there, 0.197/0.191 here), so single-site numbers also vary between forks by up to about 0.1 points.

## Timing A/B of the whole branch

`m4-devirt-tip-ab`: the base against the tip of this branch, m4, scala corpus, 20 rounds with builds alternating by fork, no profiler, patched JDK 26, `SCALA_RELEASE_OPT=1`, single-shot 4+4. Intervals are 99.9%.

| | base | tip | change |
|---|---|---|---|
| ms/op | 9,201 ±38 | 8,825 ±51 | **−4.09% ±0.64** |
| alloc/op | 5,993.9 MB | 5,999.6 MB | +0.09% ±0.31 (flat) |
| JIT compilations while measuring | 1,217 | 1,235 | +1.5% ±2.8 (flat) |
| JIT compile ms while measuring | 3,867 | 3,990 | +3.2% ±7.5 (flat) |
| deopts while measuring | 19.8 | 19.95 | flat |

The tip was faster in all 20 rounds, by 2.4% to 5.5%, and the fork means don't overlap (base 9,110–9,251, tip 8,738–8,932). The first and second halves of the campaign agree. One attempt was voided for other load (tip, round 1) and run again; its score, 8,912, was in the tip's range.

What this does and doesn't establish:
- The saving is larger than the stubs removed can explain directly. Stub share of cycles fell from 7.81% in the base's profile to 5.52% in the last prototype's, about 2.3 points, against a 4.1% gain. Those are single forks from two campaigns, with about 0.24 points of noise between forks. The inlined predicates folding into their callers would account for the difference, as suggested under "Evidence from the prototype", but nothing here measures that.
- This is the whole branch at once, so it doesn't divide the gain between the `Symbol` work and the `Type` work.
- It covers the scala corpus only, and there is no A/A of two checkouts of the same source, so a bias between the builds themselves isn't excluded.

## Microbenchmark: `SymbolOpsBenchmark`

The full compile can't resolve the individual changes, so `labs/scala2` in jvm-perf-lab now has a benchmark that amplifies them. Setup runs one real compile of the scala corpus in the fork, which fills the receiver profiles as a compile does and keeps the call sites megamorphic. Each op then applies a workload to the classes that compile left: `findMember`, `overridingPairs`, `subst`, and `erasure` (lookups after erasure, and the erasure map and other generic `TypeMap`s before it).

`m4-symops-pilot`, base against tip, m4, one fork per build and workload, kperf profile. It was cancelled after 5 of 8 cells, so `subst` has no tip cell and `erasure` has neither.

| workload | base ms/op | tip ms/op | change |
|---|---|---|---|
| `findMember` | 323.5 | 283.2 | −12.5% |
| `overridingPairs` | 516.4 | 441.7 | −14.5% |
| `subst` | 24.3 | not run | |

The `overridingPairs` tip cell's cycles and instructions per op (−55.5%, −57.6%) don't fit its time. They imply 2.04 GHz where the other cells show 3.93 GHz, so they look under-counted by about half. The cell hasn't been investigated, and only the times above are used.

`m4-symops-branch`, each source commit with the `all` workload: `findMember`, `overridingPairs`, `subst` × 16, `erasure` × 5 per op, weighted because one pass of `subst` is under 3% of the two largest passes combined. m4, patched JDK 26, no profiler, one fork per commit. The range is a local branch of the PR's five source commits without the survey commits; its `src` matches the PR's tip.

| commit | ms/op | step | against base |
|---|---|---|---|
| base | 1,471.1 | | |
| Final kind predicates, `info`, `rawInfo` | 1,383.9 | −5.9% | −5.9% |
| Final `owner`, `typeOfThis`, `tpe_*`; `mapOver`, `decls` fast paths | 1,400.7 | +1.2% | −4.8% |
| `owner` fast path under `MaxInlineSize` | 1,375.2 | −1.8% | −6.5% |
| `TypeRef.mapOver` dispatch, `typeParams`, `isTypeParameter` | 1,358.4 | −1.2% | −7.7% |
| Static `SubstMap` branches | 1,341.3 | −1.3% | −8.8% |

With one fork per commit there is no interval, and perflab marks every step flat. The first commit is the only step large enough to stand out. The later steps are 1–2%, which is the size of the fork-to-fork difference seen on the full compile, so their order and size are suggestive at most. The second and third fit the earlier JIT finding: `owner` went over `MaxInlineSize` and the fix brought it back. That agreement doesn't make either step significant on its own.

This is about twice the −4.09% of a full compile. The weights are mine, so the size of the total isn't comparable with either that or the single workloads.

## Future work

- The fast paths in "Remaining stub sites" above, starting with final `owner`, `isTypeParameter` and `typeOfThis` (the same pattern as this PR) and the `TypeRef` checks for `mapOver`/`baseType`.
- `isTerm` / `isType` / `isMethod` / `isModule` have the same API shape as `isClass` (`…SymbolApi` final overrides) and could become type checks or flag tests.
- `Symbol.typeParams`: `SynchronizedSymbol` overrides it, and the same hook pattern would apply.
