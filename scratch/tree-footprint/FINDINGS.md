# Findings: Tree and Position footprint after typer

Measured with scalac 2.13.18 on JDK 25, compiling the compiler-benchmark corpora `scala` (library, reflect and compiler; 1135 files), `scalap` and `re2s` with `-Ystop-after:typer`. Instance counts come from the live heap histogram. Sizes are given for compact object headers (`-XX:+UseCompactObjectHeaders`, 8 bytes) first and today's default 12-byte header second. The 12-byte sizes match the histogram. The 8-byte sizes are modelled with the same rule. MB here is 2^20 bytes.

## Scale

On the `scala` corpus, the live heap after typer is 334 MB with 12-byte headers. It holds 1.69M trees in 66 MB (19.7% of the heap) and 32 MB of positions and attachments (9.7%). With compact headers, trees would take 58 MB and positions 24 MB.

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
