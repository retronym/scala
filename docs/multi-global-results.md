# Several compilers in one JVM: measurements

Results of the experiments that followed [cake-diet-survey.md](cake-diet-survey.md), collected for presentation. Raw reports and profile summaries are in [multi-global-results/](multi-global-results/).

## The problem in one number

Compiling the same sources (compiler-benchmark's `re2s`) in a warm JVM on JDK 26, 2.13.x takes 92–98 ms/op when one `Global` subclass compiles everything, and 146–151 ms/op when compilations alternate between four. That is a 55–60% slowdown, and it is the situation of a build that runs Scaladoc beside the compiler, the presentation compiler, or macros that call `c.eval` (which creates a `ToolBoxGlobal`, as jsoniter-scala's macros do).

With one class, C2's type profiles at the compiler's call sites and type checks see one receiver, and it compiles them to exact class guards. With several, they go megamorphic, and the cost of the cake's shape shows: interface calls dispatch through itable stubs that scan the receiver's interfaces linearly, and interface type checks on `Global` (70 secondary supers) miss JDK 23's 63-slot hashed table and take a linear slow path.

## Changes measured

| build | change | PR |
|---|---|---|
| base | 2.13.x | |
| abstract | `Analyzer` is an abstract class; `Erasure` holds an analyzer instead of being one | retronym/scala#150 (same change as the `analyzer-abstract-class` branch, which is what was measured) |
| cake | 18 implementation-detail traits taken out of `SymbolTable`/`Types`/`Global`, `Global` 70 to 52 secondary supers | retronym/scala#151 |
| erasure | a trait's `this` is cast to its self type's class, not the member's owner trait | retronym/scala#152 |
| combo | abstract + cake | |

## Method

compiler-lab's `CompileBenchmark` (branch `compiler-lab-multi-global`) compiles `re2s` from scratch per op, in memory. `globalClasses=4` compiles with one of four synthetic `Global` subclasses per iteration, each with its own `Analyzer` class, chosen by a seeded PRNG; `globalClasses=0` uses the usual single class. Builds are compared within each fork as `@build=` variants (each build's reflect and compiler in its own class loader, iterations alternating between builds), 2 forks of 20 warmup and 20 measured 3 s iterations, JDK 26 (integration build), Apple M5 Pro. Every build compiles once with each synthetic class before warmup, so all builds' type profiles are equally polluted.

All builds compared here are bootstrapped (`restarrFull`: compiled by themselves, without the optimizer), so codegen changes such as the erasure fix take effect in the compiler's own code. Profiles are async-profiler CPU samples of the benchmark thread, one fork per build, 12 measured iterations.

## Results

All Δ are paired within forks against the first build listed; ± is the spread between the two forks.

### Each change on its own and combined, 4 `Global` classes

```
build                     ms/op    Δ vs base-boot      per fork
base-boot                 146.5
abstract-boot             103.1    -28.9% ± 0.9        -29.5  -28.2
cake-boot                 142.5     -3.3% ± 3.0         -1.2   -5.4
erasure-boot              145.8     -3.1% ± 2.7         -1.2   -5.0
combo-boot (abstract+cake)100.3    -31.7% ± 2.0        -33.1  -30.3
combo + erasure           101.4    -32.6% ± 2.2        -31.1  -34.2
```

(Rows come from three campaigns, each with `base-boot`: `abstract-cake-boot-g4`, `cake-only-boot-g4`, `combo-erasure-boot-g4`. `base-boot` measured 146.5, 148.5 and 150.6 in them; each row's Δ is against its own campaign's.)

### The same, 1 `Global` class

```
build                     ms/op    Δ vs base-boot
base-boot                  93.3
abstract-boot              93.3     +0.3% ± 0.3
cake-boot                  95.8     -0.8% ± 1.4
erasure-boot              101.2     +1.8% ± 1.0
combo-boot                 94.0     +0.4% ± 0.8
combo + erasure            99.5     +0.1% ± 1.5
```

(Rows from the same three campaigns' `g0` counterparts; `base-boot` measured 93.3, 97.1 and 99.4 in them. Absolute ms/op differ between campaigns: forks carrying four builds' compiled code run at about 99 rather than 93. The paired Δ is what to read.)

### Where the time goes

Share of benchmark-thread CPU, from the profiles:

```
                          base-boot        abstract-boot     combo+erasure
globalClasses              4       1        4       1        4       1
ms/op (profiled)        147.7    97.6    112.4   100.6    101.1    96.0
itable stubs            29.1%    1.7%     3.9%    1.3%     1.3%    1.3%
secondary supers slow    2.7%    0.5%     4.8%    0.7%     0.3%    0.1%
vtable stubs             3.7%    5.6%     5.8%    5.6%     6.7%    5.5%
```

- base-boot, 4 classes: the itable stubs are on the four analyzer classes and `Global$erasure$` (21.5% and 6.5% in the STARR-compiled base profile), called from `Typer.typed`, `ImplicitSearch.isPlausiblySubType`, `Context.make`, `Context.isAccessible`: every call from the typechecker to its analyzer is `invokeinterface Analyzer.m`, and `Analyzer` is the last entry in the analyzer's itable.
- abstract-boot, 4 classes: those are gone. What remains is itable stubs on the `Global` subclasses themselves (2.6%), from `TypeComparers.isTrue` (`checkcast Types; invokeinterface Types.isErrorOrWildcard`) and `Names$TypeName.body`, which are calls through a trait's `this` to sibling traits (the erasure fix's target), and the secondary supers slow path (4.8%), called from `Names$Name.$outer`, `TypeComparers.isSubType1`, `firstTry`, `fourthTry`, `thirdTryRef`, `isTrue`: interface casts on `Global`, whose table is full (the cake diet's target).
- combo+erasure: no itable stubs on compiler receivers remain (the 1.3% left is collections: `Map$Map2`, list and iterator adapters), the slow path is down to 0.3%, mostly `Names$Name.$outer`. vtable stubs (5.5–6.7%) are megamorphic virtual calls on `Type` and `Symbol` subclasses (`dealiasWiden`, `isTopLevel`, `walkBaseClasses`, `mapOver`), the same with one class as with four; with four, the analyzer also appears as a vtable receiver (about 0.3% per class).

No other cake component behaves like `Analyzer` did. A static count of the remaining `invokeinterface` sites finds `Parsers`, `JavaParsers`, patmat's and async's traits, but their receivers are objects nested in `Global` (one class however many `Global` subclasses), so their call sites stay monomorphic and do not appear in the profiles; `ContextErrors` (234 sites, receivers the analyzer classes) does not appear either, its calls being on error paths.

### The mechanism in isolation

`CakeSupersBenchmark` (retronym/scala#153) is a cake of N self-typed traits whose `Hot` trait calls eight siblings, in today's shape (`checkcast Trait; invokeinterface`, `viaTrait`) and in the erasure fix's (`checkcast U; invokevirtual`, `viaClass`), with receivers from 1 or 4 classes. ns per call, JDK 26:

```
traits  classes   viaTrait   viaClass
    60        1       1.92       1.89
    60        4      10.23       2.01
    70        4      44.60       2.13
   100        4      61.47       2.07
```

JDK 21.0.12, which has the hashed table backported (`UseSecondarySupersTable` is on), is worse above the table size: 81.6 ns at 70 traits.

## Conclusions

- `Analyzer` as a class is the change that matters: about -29% when several `Global` subclasses share the JVM, nothing with one. It is two files.
- The cake diet adds about 3 points on top, by taking `Global` under the secondary supers table size; on its own it is worth about as much. Its value is that and the headroom; its cost is churn in `SymbolTable`.
- The erasure fix is worth about 3% on its own and nothing on top of the other two in scalac, but it removes the cost entirely for self-typed cakes in general, in the microbenchmark below and above the table size.
- With all three, several compilers in one JVM cost about 5% over one, instead of 55–60%.

## Earlier runs, superseded

Two earlier comparisons let the builds' type profiles see different numbers of `Global` subclasses (the per-iteration choice was shared by the variants, so a build whose iterations drew fewer classes was measured less polluted, identically in every fork). They are kept for the record in [multi-global-results/campaigns](multi-global-results/campaigns) with `BIASED` in the name: `multi-global-builds-g4` (STARR-compiled abstract -28.6%, abstract+cake -33.6%, in line with the clean results) and `erasure-boot-g4` (erasure -5%, then +10% in a four-build rerun; the clean figure is -3.1%). compiler-lab now compiles with every class before warmup.

The first single-tree runs (`multi-global-jdk26`, `multi-global-jdk21`: one build per fork, so not affected) showed the slowdown is the same on JDK 21 and JDK 26, and the same with 4 and with 8 classes:

```
globalClasses       0       1       4       8
JDK 26           97.6    95.5   151.7   146.1
JDK 21           98.1    91.7   146.8   145.8
```

Made with an LLM.
