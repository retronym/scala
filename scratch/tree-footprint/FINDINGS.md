# Findings: Tree, Position, Symbol, Type and Name footprint after typer

Measured with scalac 2.13.18 on JDK 25, compiling the compiler-benchmark corpora `scala` (library, reflect and compiler; 1135 files), `scalap` and `re2s` with `-Ystop-after:typer`. Instance counts come from the live heap histogram. Sizes are given for compact object headers (`-XX:+UseCompactObjectHeaders`, 8 bytes) first and today's default 12-byte header second. Each corpus was compiled once with each header size, and the modelled size of every class matches the histogram for both. MB here is 2^20 bytes.

## Scale

On the `scala` corpus, the live heap after typer is 333 MB with 12-byte headers. It holds 1.69M trees in 66 MB (19.7% of the heap) and 32 MB of positions and attachments (9.7%). With compact headers, trees would take 58 MB and positions 24 MB.

An identity-deduplicated walk of the unit bodies reaches 1.33M of the 1.69M trees. The rest are held elsewhere, for example by symbols' completers and annotations. Weighting by the walk instead of the heap changes the percentages below by at most 0.3 points.

`Select` and `Ident` take about 40% of tree bytes. `TypeTree` comes next, followed by `Apply`.

## Layout

Every tree starts with an object header and four 4-byte fields: `$outer` (in `TreeContextApiImpl`, pointing to the `SymbolTable`), `id`, `rawtpe` and `rawatt`. That is 24 bytes with compact headers and 28 with 12-byte headers. SymTrees add `symbol`, and each case class adds its own fields.

Objects round up to 8 bytes, so removing a field only saves memory when it removes a padding slot. Adding one only costs memory when no padding slot is free. With compact headers, a class with an even number of 4-byte fields has no padding. With 12-byte headers, a class with an odd number has none. Which classes a candidate helps therefore depends on the header size.

| Class | Today, 8 B header | Today, 12 B header |
|---|---|---|
| `Ident` | 32 | 40 |
| `Select` | 40 | 40 |
| `TypeTree`, `Apply` | 32 | 40 |
| `Literal` | 32 | 32 |
| `DefDef` | 56 | 56 |

## Candidates

- A: drop `Tree.id`. It is read only for diagnostics: position validation errors, `TreeBrowsers` and the tree printer. An identity map built when an error is reported, or `System.identityHashCode`, can serve them.
- B: move `symbol` from `SymTree` to `Tree`, so every tree has the field. The aim is to read the symbol without a virtual call, and to let `TermTree` and `TypTree` become classes.
- A+B: both changes together.
- C, for reference only: drop `$outer`, the other per-tree field.

Change in live tree bytes, against today's layout with the same header:

| Corpus | Header | A | B | A+B | C | A+B+C |
|---|---|---|---|---|---|---|
| scala | 8 B | −9.1% | +7.0% | −7.8% | −9.1% | −15.1% |
| scala | 12 B | −11.5% | +1.2% | −5.3% | −11.5% | −18.4% |
| scalap | 8 B | −8.9% | +7.9% | −7.2% | −8.9% | −14.3% |
| scalap | 12 B | −11.8% | +1.5% | −4.8% | −11.8% | −18.1% |
| re2s | 8 B | −10.3% | +6.2% | −7.8% | −10.3% | −16.2% |
| re2s | 12 B | −10.9% | +2.1% | −5.3% | −10.9% | −17.9% |

With compact headers:

- A shrinks the classes with an odd field count by 8 bytes: `Select` 40→32, `ValDef` 48→40, `DefDef` 56→48, `Literal` and `New` 32→24, and also `CaseDef`, `If`, `Bind` and `Function`. `Ident`, `TypeTree`, `Apply` and `This` are already full at 32 and gain nothing.
- B leaves SymTrees unchanged. Non-SymTrees with an even field count grow from 32 to 40: `TypeTree`, `Apply`, `TypeApply`, `Block` and `AppliedTypeTree`. `Literal`, `New`, `CaseDef` and `If` fit the new field into their padding.
- A+B gives back only 1.4 points of A's saving. `TypeTree`, `Apply` and `Select` all end at 32. The classes that lose A's saving are non-SymTrees with an odd field count, such as `Literal` and `CaseDef`.

With 12-byte headers:

- A shrinks the classes with an even field count from 40 to 32: `TypeTree`, `Apply`, `Ident`, `This`, `TypeApply` and `Block`. `Select`, `ValDef`, `DefDef` and `Literal` gain nothing.
- B fills the padding of `TypeTree`, `Apply` and others, and grows `Literal`, `New`, `CaseDef`, `If` and `Throw` by 8 bytes.
- A+B gives back 6.2 points of A's saving, because `TypeTree` and `Apply` return to 40.

C has the same effect on padding as A. Tree methods reach the universe through `$outer`, so dropping it would need them to get the universe some other way.

## B beyond footprint

Several trees that are not SymTrees override `symbol` to delegate to a child rather than store it:

- `Apply` and `TypeApply` return `fun.symbol`.
- `Super` returns `qual.symbol`.
- `AppliedTypeTree` returns `tpt.symbol`.
- `TypeTree` returns `typeTreeSymbol(this)`.
- `ReferenceToBoxed` and nsc's `DocDef` also delegate.

A plain field read in `Tree` only replaces the virtual call if these classes store the delegate's symbol. Their setters already write through to the child, but `TypeTree`'s symbol follows its `tpe`. Otherwise they keep an override and the call site stays polymorphic. `Apply` and `TypeApply` are 11.7% of trees.

## Positions

| Class | Fields | 8 B header | 12 B header | Live (scala) |
|---|---|---|---|---|
| `RangePosition` | source, start, point, end | 24 B | 32 B | 731,741 |
| `OffsetPosition` | source, point | 16 B | 24 B | 258,994 |
| `SingleAttachment` | pos, att | 16 B | 24 B | 124,712 |
| `TransparentPosition` | as `RangePosition` | 24 B | 32 B | 37,454 |
| `NonemptyAttachments` | pos, all | 16 B | 24 B | 3,052 |

With 12-byte headers every position and attachment class carries 4 bytes of padding. One more field would cost nothing, and removing one would save 8 bytes. With compact headers none of them has padding, so one more field would cost 8 bytes and removing one would save nothing. Compact headers save 8.8 MB of positions and attachments on the `scala` corpus. There are fewer positions than trees because trees share position objects, through `copyAttrs` and `focus`.

## Symbols, types, names and scopes

After typer the `scala` corpus holds 80 MB of symbols, types, names and scopes with compact headers (85 MB with 12-byte headers), 24% of the live heap. Each family includes the objects and arrays it owns.

| Family | 8 B header | 12 B header | Largest parts |
|---|---|---|---|
| Type | 42.0 MB | 43.8 MB | `ClassArgsTypeRef` (331k), `BaseTypeSeq` with its `elems`, `typeSymbols` and `pending` |
| Symbol | 27.4 MB | 29.9 MB | `TermSymbol` (166k), one `TypeHistory` per symbol (277k), `MethodSymbol` (57k) |
| Scope | 6.8 MB | 7.4 MB | `Scope` (72k, 86% empty), `ScopeEntry`, hash tables of large scopes |
| Name | 3.9 MB | 3.9 MB | the 1M-character name table (595k used), 53k names |

`scalap` and `re2s` are small: 8.4 MB and 4.9 MB with compact headers. Most of their symbols come from the classpath, so symbols make up a larger share than on `scala`.

A reflective walk of everything reachable from `Global` counted, for each field of each class, how many instances hold something other than null, zero or a sentinel such as `NoSymbol`, `Nil`, `NoPrefix` or `EmptyScope`. Its instance counts match the histogram for every one of these classes.

## Layout

Symbols carry an 8-byte `_rawflags`. The model places fields as HotSpot does on JDK 15 and later: primitives largest first, then references, with each field going into the smallest gap that fits it, including gaps left by the header or a superclass. The gap before `_rawflags` holds `$outer` with 12-byte headers, and `$outer` and `id` with compact headers.

| Class | Fields after the header | 8 B header | 12 B header |
|---|---|---|---|
| `TermSymbol` | 13 × 4 B + `_rawflags` | 72 (4 B padding) | 72 (none) |
| `MethodSymbol` | `TermSymbol` + 4 `mtpe*` cache fields | 88 (4 B padding) | 88 (none) |
| `TypeHistory` | `_validFrom`, `_info`, `_prev`, `$outer` | 24 | 32 (4 B padding) |
| `ClassArgsTypeRef` | 10 × 4 B + 1-byte `trivial` | 56 (7 B padding) | 56 (3 B padding) |
| `MethodType` | 3 × 4 B + 2 flags | 24 | 32 |
| `TermName` | `index`, `len`, `cachedString`, `next`, `$outer` | 32 (4 B padding) | 32 (none) |

With compact headers, `TermSymbol` and `MethodSymbol` have a free slot, so one fewer field saves 8 bytes on each. With 12-byte headers they have none, and a single field gains nothing on them. `ClassArgsTypeRef` is one byte over 48 with compact headers, which is the `trivial` flag. With 12-byte headers it needs two fewer 4-byte fields to shrink.

## Rarely set fields

After typer on `scala`:

- `Symbol.initName` is set on every symbol. It is the constructor parameter, kept as a field only because `sealedSortName` reads it.
- `Symbol.javaBinaryNameStringCache` is null on every symbol.
- `TypeHistory._prev` is null on every history: each symbol has exactly one `TypeHistory`.
- `TermSymbol._referenced` is set on 0.1% of `TermSymbol`, 2.8% of `MethodSymbol`, and every `ModuleSymbol` (its module class).
- `TypeRef.parentsCache` and `parentsPeriod` are set on 0.2% of `ClassArgsTypeRef` and 1.7% of `ClassNoArgsTypeRef`.
- Every `BaseTypeSeq` allocates a `pending` bit set and its `long[]`, 2.6 MB in all. It is only used while a lazy intersection element is being computed.
- `Name.cachedString` is set on 2.5% of names.
- `Symbol._privateWithin` (0.5% of `TermSymbol`), `Symbol._annotations` (1.3%) and the `CompoundType` base type and base class caches on `RefinedType0` (under 3%) are also rarely set, but they are read often, so a side table would cost lookups on hot paths. They are not modelled.

## Candidates for symbols, types and names

None is implemented.

- S1: drop `Symbol.initName`. Its readers, the pickler and exhaustivity checking, run before names are expanded, so `rawname` may give the same order. This needs checking, since a different order would change pickles.
- S2: move `javaBinaryNameStringCache` from `Symbol` to `ClassSymbol`. Its readers are the pickler's `.sig` writer and the backend, for classes and modules. A module can compute it from its module class.
- S3: inline the head of the type history into `Symbol`. Replace `infos` with the info, its valid-from period and a link to older entries. Each symbol grows by 8 bytes and loses its `TypeHistory` object. Later phases add history entries, which would still be separate objects.
- S4: move `TermSymbol._referenced` to `ModuleSymbol`, and keep the rarer uses (default getters at typer, then accessors, outer fields and specialized members in later phases) in an attachment.
- T1: move the `TypeRef` parents cache to a side table. This saves 8 bytes on every `TypeRef` with either header.
- T2: allocate `BaseTypeSeq.pending` lazily. The figure assumes no sequence ever needs it, so it is an upper bound.
- N1: drop `Name.cachedString`.
- X, for reference only: drop the cake `$outer` from symbols, types, names and type histories.

Change in bytes of symbols, types, names and scopes on `scala` (80.1 MB with 8 B headers, 85.0 MB with 12 B headers):

| Candidate | 8 B header | 12 B header |
|---|---|---|
| S1 | −1.8 MB (−2.2%) | −0.4 MB (−0.4%) |
| S2 | −1.7 MB (−2.1%) | −0.3 MB (−0.3%) |
| S3 | −4.2 MB (−5.3%) | −6.4 MB (−7.5%) |
| S4 | −1.7 MB (−2.1%) | 0 |
| S1 to S4 | −8.0 MB (−9.9%) | −8.4 MB (−9.9%) |
| T1 | −3.0 MB (−3.8%) | −3.0 MB (−3.5%) |
| T2 | −2.6 MB (−3.2%) | −2.6 MB (−3.0%) |
| N1 | −0.4 MB (−0.5%) | 0 |
| All but X | −13.9 MB (−17.4%) | −14.0 MB (−16.4%) |
| X | −6.2 MB (−7.8%) | −3.8 MB (−4.5%) |

S1 to S4 together cut symbol bytes by 29% with compact headers and 28% with 12-byte headers. With 12-byte headers S1, S2 and S4 each do little alone, because `TermSymbol` and `MethodSymbol` have no padding to free. Together they remove 12 bytes, which pays for S3's 8 and leaves `TermSymbol` at 72 bytes without its 32-byte `TypeHistory`. With compact headers, `TermSymbol` ends at 64.

On the other corpora, S1 to S4 save 12.0% (`scalap`) and 12.5% (`re2s`) with compact headers, and all the candidates together save 15.4% and 14.9%. T1 and T2 matter less there, because those corpora create few types of their own.

Of the total, S3 and T1 are the largest wins that do not depend on the header size, and S3 also removes 277k objects from the heap.
