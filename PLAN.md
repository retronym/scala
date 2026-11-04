# A UTF-8 name table for scalac

Status: measurements done on the `scala`, `scalap` and `re2s` corpora; awaiting a decision before any implementation. The `scalap`/`re2s` CPU and allocation profile is queued.

## Problem

`Names` keeps every name in one growing `Array[Char]`. A `Name` is an offset and a length into it, found through two fixed 32k-bucket hash tables. The proposal is to store UTF-8 bytes in that array, for two reasons: names would interoperate better with Java strings, and the table would take less memory.

The measurements below do not support either reason as stated. The memory saved is under 0.5% of the live heap, and the JDK offers no zero-copy route from bytes to `String`. What they do show is a smaller, real cost that a byte table does not remove: `Name.toString` recomputing the same strings, and temporary arrays on name entry.

## What is at stake

Counts come from an instrumented `Names` overlaid on a quick build, in one cold compile per corpus on JDK 25. Shares come from the base forks of `m5-direct-compile-prof` and `m5-backend-prof` (testbed m5-pro, JDK 25, benchmark thread only, one fork each, so small differences mean nothing).

| | `scala` | `scalap` | `re2s` |
|---|---|---|---|
| Term names / type names | 89,087 / 14,712 | 12,578 / 6,497 | 9,175 / 4,657 |
| Mean name length | 19.1 | 17.9 | 16.3 |
| Table used / capacity as `char[]` | 3.40 MB / 4.19 MB | 449 KB / 524 KB | 299 KB / 524 KB |
| ASCII share of the table | 99.9998% | 100% | 100% |
| Live heap after the compile, `Global` alive | 470 MB | 31 MB | 20 MB |
| Allocated by the compile | 6.2 GB | 273 MB | 161 MB |

On `scala`, halving the table saves 2.1 MB of capacity (0.45% of the live heap) and about 4 MB of allocation across array growth (0.06%). The `Name` objects themselves take 3.3 MB and are unchanged.

| Share of the benchmark thread | Full compile of `scala`, CPU | Allocation | Backend only, CPU | Allocation |
|---|---|---|---|---|
| Any frame under `Names` | 1.30% | 1.62% | 2.85% | 2.84% |
| `Name.toString` | 0.15% | 0.70% | 1.78% | 2.79% |
| `newTermName` family | 0.47% | 0.60% | 0.48% | 0.06% |
| `toTypeName` / `toTermName` | 0.20% | 0.01% | 0.13% | 0% |
| Char scans (`pos`, `startsWith`, `charAt`...) | 0.29% | 0.05% | 0.19% | 0% |
| `Codec.toUTF8` in the pickler | 0.01% | 0.19% | 0% | 0% |
| `getUTF` in the classfile reader | 0.02% | 0.01% | 0% | 0% |

The 1.3% and 1.6% are the ceiling for any change to name storage on a full compile. I take the 1.51% `toString` frame in your backend flame graph to be the same cost as the 1.78% here, from another fork or view. In a full compile its CPU share is about a tenth of that.

The `scala` corpus compiles the library from source, so it reads no pickles. `scalap` and `re2s` do, and their share of the `Codec.fromUTF8` path is what the queued campaign `m5-names-baseline-prof` will show. By count, pickled names are 11,575 of 61,081 lookups on `scalap`, 80% of them hits.

Entry points on `scala`, calls and misses:

| Entry | Calls | Misses | Mean length |
|---|---|---|---|
| `char[]` (scanner, classpath names) | 721,122 | 27,702 | 5.1 |
| `String` | 510,221 | 52,203 | 9.0 |
| `String`, cached | 72,561 | 9,123 | 51.2 |
| `subName` | 327,193 | 59 | 8.2 |

`toString` is called 1,334,639 times on 53,179 distinct names, so 96% of the calls rebuild a string already built once. Only 34% of those calls hit the existing `cachedString`.

## Findings

**Readers of the table.** Nothing in the repository reads `chrs` outside `Names.scala`; the bridge in `src/sbt-bridge` already uses `appendTo`. `lookupTypeName` has no callers outside its test. `Name.start` is used as an identity and hash (`Scopes`, `Trees.sameName`, `Name.hashCode`) and as a dense index for keywords (`Scanners.kwOffset`, `JavaScanners`), so it must stay unique per string, but it need not be a real offset. `length`, `charAt`, `pos` and `subName` all count UTF-16 units and are public API through `CharSequence`.

**External users.** `chrs` has been deprecated since 2.12.9 and `scala.reflect.internal` is excluded from MiMa. The one user found is Zinc's source-compiled bridge, `xsbt/CallbackGlobal.scala:256,261`. sbt resolves the prebuilt `scala2-sbt-bridge` where `ScalaArtifacts.hasScala2SbtBridge` holds, so that source only meets older compilers. GitHub code search found nothing in scalameta, metals, scoverage, wartremover, Ammonite or kind-projector; I ran out of API quota before scalafix, and search does not prove absence. Removing `chrs` looks acceptable. Plugins using `Name.start` are not ruled out.

**The JDK.** I read `String.java` on 17, 21 and 25. `new String(bytes, off, len, UTF_8)` runs `countPositives` and then `Arrays.copyOfRange`; `ISO_8859_1` skips the scan. Both copy, and the result is the same size as one built from `char[]`. A rough loop on 16 ASCII names (8 to 15 chars, on efficiency cores beside a running campaign, so ratios only, with 6 to 7 ns of loop overhead) gave these figures:

| ns/op | JDK 17 | JDK 21 | JDK 25 |
|---|---|---|---|
| `new String(char[])` | 13.5 | 15.0 | 17.8 |
| `new String(bytes, ISO_8859_1)` | 17.7 | 18.9 | 17.2 |
| `new String(bytes, UTF_8)` | 23.3 | 22.0 | 21.8 |
| Cached `String` field | 7.0 | 5.8 | 6.8 |

So for names this short, `StringUTF16.compress` is no slower than the byte routes, and a byte table does not make `toString` cheaper. Only not building the string does. JDK 8 has no compact strings and no ranged `Arrays.equals`, and the build targets 8; I have not measured it because it isn't installed here.

**Conversions that could go.** With a byte table, the unpickler's `ByteBuffer`, `CharBuffer` and `char[]` for ASCII names, and the pickler's `String`, `CharBuffer` and `byte[]`, become hash-and-compare and a copy. `toString` stays a copy. The classfile reader yields `String`s through `DataInputStream.readUTF` whatever the table holds, and they become names lazily. scala-asm takes `String` and encodes it again, so the backend gets nothing without changing scala-asm.

**Encodings.** Classfiles use modified UTF-8 (NUL as two bytes, supplementary characters as two 3-byte surrogates). Pickles use standard UTF-8 through `Codec`, which turns a lone surrogate into a replacement. `subName` can split a surrogate pair, which standard UTF-8 cannot represent, so a true UTF-8 table would have to use the modified form internally and could not copy non-ASCII pickle bytes verbatim. Encoded operators are ASCII (`$colon$colon`, `$u2192`), which helps compactness.

**The hash.** `hashValue` is `31 * h + c` over UTF-16 units, which is exactly `String.hashCode`, masked to 15 bits. `String` entries could reuse the JDK's cached hash, and today they copy to `char[]` instead. Over bytes the hash agrees only for ASCII, so non-ASCII byte entries must decode as they hash. Lookups average 0.65 probes on `scala`.

**Non-ASCII code bases.** I rewrote every identifier that `scalap` and `re2s` define into CJK characters, keeping each first character so that patterns still mean the same, and compiled them without errors.

| Synthetic corpus | Non-ASCII names | Table ASCII | `char[]` used | UTF-8 | Latin-1 or ASCII bytes, with fallback at 2 bytes per char |
|---|---|---|---|---|---|
| `scalap` | 16% | 92.5% | 451 KB | 259 KB | 242 KB |
| `re2s` | 19.5% | 90.4% | 300 KB | 178 KB | 164 KB |

Classpath names keep the table mostly ASCII, so UTF-8 still shrinks it, and it only passes `char[]` when nearly everything is non-ASCII (the limit is 1.5 times `char[]`). The loss is in time: on synthetic `re2s`, 64% of `charAt`, 93% of `endsWith(String)` and 96% of `pos` calls land on a non-ASCII name, where a UTF-8 table has no O(1) index. Many of these could work on bytes, but `charAt` and every returned index could not, and `encode` calls `charAt` per character.

## Designs

| | Design | Saves | Costs |
|---|---|---|---|
| A | Pure UTF-8, `charAt` by scanning | 2.1 MB on `scala`, the unpickler and pickler conversions | Quadratic scans on non-ASCII names, surrogate handling, a slower `toString` |
| B | ASCII bytes in the table, each non-ASCII name holds a `String` | As A, with O(1) `charAt` everywhere | Two paths through every operation, canonicalising `subName`, no help for `toString` |
| C | As B with Latin-1 in the table | A few more names on O(1) bytes (3 of 89,087 on `scala`) | High bytes are not UTF-8, so entry still checks for ASCII |
| D | Each name holds a `String`, as Scala 3's `SimpleName` does | All `toString` allocation, `char[]` temporaries, `compress`; `toTypeName` rehashing through the cached hash; one code path; JDK-intrinsified scans | About 1.5 MB more retained on `scala` (0.3% of live), `subName` no longer shares, heavier on JDK 8 |
| E | Keep `char[]`, cache `toString` on first use, enter from `String` without a temporary | Most of the `toString` row, the temporaries | About 3 MB more retained on `scala` if every string is kept; nothing in the table |

D's retained estimate: 89k `String`s at about 64 bytes (5.7 MB) replace the 4.19 MB array. A `Name` stays at 32 bytes, because dropping `len` only leaves padding.

I recommend against A and C. Of the rest I expect D to win, with E as the cheap control that shows how much of D is just the cache. B is what meets the original goal most closely, and I expect it to measure at or below the noise floor: its only time savings are on the unpickler and pickler paths, and it adds a branch to every operation. Everything here is bounded by the 1.3% CPU and 1.6% allocation above, so even D is a modest change on a full compile, and worth more on the backend and for simplicity than for speed.

The scanner's `cbuf.toCharArray` per identifier (721,122 calls) is independent of all of these and could be dropped regardless.

## Prior art

- [scala/scala#7976](https://github.com/scala/scala/pull/7976), a String-backed `Names` with its own holder objects (2019). Promising microbenchmarks, failed with an OOME in bootstrap from an oversized map, closed for inactivity; the author noted the hot paths had since been improved.
- [scala/scala#8168](https://github.com/scala/scala/pull/8168), interner bake-off (2019). It measured `src/compiler` at 2.8 MB used plus 3.0 MB slack, in line with today.
- `retronym/scala` `faster/newTermName` (2020-04), direct entry from `String` with no temporary, unmerged and still applicable. `topic/avoid-name-to-string` (2019-05) reduced `toString` calls by hand.
- `retronym/scala` `topic/name-table-sharing` and `topic/decake-names-2` (2019), sharing a table across `Global`s, a different goal.
- [scala/scala#8894](https://github.com/scala/scala/pull/8894), copying `_chrs` to a local before loops, merged.
- Scala 3 holds a `String` in `SimpleName` and has simplified its table to a `HashMap[String, SimpleName]` ([scala/scala3#27074](https://github.com/scala/scala3/pull/27074), no performance change). You suggested Strings and JEP 254 in 2016 on [scala/scala3#1527](https://github.com/scala/scala3/issues/1527#issuecomment-252576957).
- I found no earlier proposal for UTF-8 bytes in the table.

## Parked

- Resizing the hash tables (fixed 32k buckets, load 2.7 on `scala`).
- Linking each term name to its type name, which removes `typeHashtable`.
- Passing bytes through to scala-asm's constant pool, which belongs with the direct emission work.
- Sharing a table across `Global`s.

## DONE

- Branch `claude/utf8-name-table` from `upstream/2.13.x`.
- Audit of readers and external users; JDK sources for 17, 21 and 25.
- Table size, ASCII share, entry and operation counts, on real and synthetic corpora.
- CPU and allocation shares from existing m5-pro profiles on `scala`.

## TODO

- Read `m5-names-baseline-prof` for the unpickler's share on `scalap` and `re2s` when the queue reaches it.
- Decision on which designs to spike.
- Spike, each commit compiling: E, then the chosen table design. Bootstrap with `restarrFull`.
- A proper JMH run of the string paths in place of the rough loop, including JDK 8 if it can be installed.
- Numbers from `perflab` on m5-pro with an A/A, on `scala`, `scalap`, `re2s` and `BackendBenchmark`, then a view on whether it is worth having.
