# The cost of entering owners in tree transformers

Most phases after the typer are tree transformers that do nothing to most of the trees they visit. What they pay for every tree regardless is the bookkeeping of descending: tracking the current owner and, for typing transformers, a typer context for it. This note records what that bookkeeping costs, what lazy contexts in `TypingTransformer` save, why the by-name `atOwner` blocks make the savings depend on the JIT, and a closure-free protocol, `pushOwner` / `popOwner`, that removes the dependence.

## Lazy contexts in typing transformers

`TypingTransformer.atOwner` used to make a new `Context` for every owner it entered: every class, method, val and function, in each of the dozen or so phases built on it. Few of those contexts were read. The change makes them lazily: entering an owner pushes the tree and owner onto two arrays, and the contexts of the pending frames are made, by the same `Context.make` calls as before, when `localTyper` is next read. `-Yeager-typing-transformer-contexts` restores the eager behaviour, so that the two can be compared within one JVM.

Compiling the corpora of scala/compiler-benchmark, lazy against eager, paired within forks (four forks each):

| corpus | allocation per compile | time per compile |
|---|---|---|
| re2s | 89.7 MB to 87.7 MB, −2.26% ± 0.01 | +0.5% ± 2.4 (one disturbed fork; the other three −0.3% to −1.1%) |
| scala (library, reflect, compiler) | 6836 MB to 6692 MB, −2.11% ± 0.04 | −0.95% ± 0.51 |

These were measured on a quick build without the optimizer (see below), and the eager variant is not quite the old code: it runs the new `atOwner`, with its try/finally. Both make the comparison conservative.

## Build quick with the optimizer before measuring

Releases are compiled with `-opt:l:inline -opt-inline-from:scala/**`; local builds are not. The difference matters for exactly this kind of code. In quick, `Transformer.transformTrees`, `transformStats` and the other list helpers allocate a closure for `mapConserve` on every call; in the released scala-reflect, `mapConserve` is inlined and the closure is gone. A benchmark of quick without the optimizer measures a compiler nobody ships. compiler-lab's README says how to build quick as releases are (a `local.sbt`).

## A typing transformer that does nothing

`TypingTransformerBenchmark` (in compiler-lab) isolates the fixed cost. Setup compiles a corpus up to the typer, once; one op runs a typing transformer that changes nothing over every unit. Its transformers are made once and reused, so with nothing typed an op should allocate nothing at all. On re2s, with the optimizer, eager against lazy:

| | time per pass | allocation per pass |
|---|---|---|
| eager | 0.227 ms | 265 KB |
| lazy | 0.191 ms | 56 KB |

The contexts account for the difference. The 56 KB that remains is not contexts: an allocation profile attributes all of it to the closures passed to `atOwner`, 89% from `ValDef.transform`'s `atOwner(symbol) { treeCopy.ValDef(...) }` and the rest from `TypingTransformer.transform`'s own `atOwner(currentOwner) { tree.transform(this) }`.

A by-name block costs nothing only when the JIT inlines the whole chain of `atOwner` overrides into the method that makes the closure, and then scalar-replaces the closure. Whether it does depends on inlining budgets, and on how hot and how large the callers are. `ValDef.transform` is compiled on its own with the chain inlined, but where it is itself inlined into the large `TypingTransformer.transform`, the chain no longer fits and the closure escapes. Small changes move the result. Without the try/finally that the lazy change had put around `super.atOwner`, the pass allocated 24 KB less in both variants; with it removed but `atOwner` not also split into small `enter` and `exit` methods, it allocated as much as before. An exception handler, a few more bytecodes, another override: any of them can turn a free closure into an allocation per owner, in every typing transformer phase.

## A toy

`OwnerProtocolToy` (in compiler-lab) distils this: trees of definitions and expressions, a transformer that tracks the owner, and a context per owner kept eagerly, lazily or not at all. Definitions enter their owner either with an `atOwner` block or with `pushOwner` / `popOwner`. A pass over 20,000 definitions, nested up to six deep, changing nothing, one variant per JVM:

| contexts | protocol | allocation per pass | with `atOwner` not inlined |
|---|---|---|---|
| none | `atOwner` block | 0 | 480 KB |
| none | push / pop | 0 | 0 |
| eager | `atOwner` block | 480 KB | 960 KB |
| eager | push / pop | 480 KB | 480 KB |
| lazy | `atOwner` block | 0 | 480 KB |
| lazy | push / pop | 0 | 0 |

The toy is small enough that C2 inlines everything, and every closure is scalar-replaced, with or without a try/finally. Preventing the inlining of `atOwner` (`-XX:CompileCommand=dontinline,...::atOwner`), which is what happens to the hot paths in the compiler, makes every closure escape: 24 bytes per definition, as much as an eager context costs in the toy. Push / pop allocates nothing either way. Preventing the inlining of `Tree.transform` instead changes nothing, because each definition's `transform` makes and consumes its closure itself.

In the toy the time per pass is the same for all variants within noise: allocating 20,000 small objects is cheap next to the traversal. In the compiler the cost is in the garbage across a whole compile, as the allocation figures above show.

## `pushOwner` / `popOwner`

`InternalTransformer`, the base of every transformer in the compiler, gains `pushOwner(owner)`, which enters `owner` and returns the previous owner, and `popOwner(prev)`, which leaves it; its `atOwner` becomes `pushOwner`, the block, `popOwner`. The trees that enter owners (`PackageDef`, `ClassDef`, `ModuleDef`, `ValDef`, `DefDef`, `TypeDef`, `Function`) call the pair around their children instead of passing a closure to `atOwner`, as do `transformStats` for an expression owner and `TypingTransformer.transform` for a `Template`. `TypingTransformer` overrides the pair to push and pop its frames. The protocol is in `scala.reflect.internal` rather than on `scala.reflect.api.Trees#Transformer`, which would need new public API in scala-reflect and so fail MiMa's forward check; trees test for an `InternalTransformer` and use `atOwner` for any other transformer.

Transformers that override `atOwner` must still see every owner through their override: `SuperAccessors` tracks `validCurrentOwner` and a map of typers per owner there, and compiler plugins may do the like. So trees use push / pop only for transformers whose nearest declaration of `atOwner` is in a class that also declares `pushOwner`, keeping the two in step (`InternalTransformer`, `TypingTransformer`); for others they call `atOwner` with a closure, as before. The check is reflective, once per transformer class (a `ClassValue`), and kept in a final field that the trees read. The toy's `legacy` variant, an `atOwner` override in the style of `SuperAccessors`, works this way, at the cost of its closure; `SuperAccessors` itself could move its bookkeeping to the pair.

As with the eager code, an exception thrown while transforming leaves the owner state as it was at the throw. A finally around each pair would no longer cost a closure, but there was none before and the phases do not recover from such exceptions.

With the protocol, `TypingTransformerBenchmark` on re2s, optimized quick:

| | time per pass | allocation per pass, nothing typed | allocation per pass, a literal typed every 16 methods |
|---|---|---|---|
| eager | 0.216 ms | 209 KB | 211 KB |
| lazy | 0.177 ms | 2 B | 8.6 KB |

The no-op pass with lazy contexts allocates nothing, and the eager figure is now the contexts alone.

## Future work

- Repeat the whole-compile comparison of eager and lazy contexts on an optimized quick, with the protocol.
- Move `SuperAccessors`' `atOwner` override to `pushOwner` / `popOwner`.
- `Traverser.atOwner` has the same shape and the same exposure.
- Where scala-reflect may add API, the protocol could move to the public `Transformer`, for macros and other users of the API.
