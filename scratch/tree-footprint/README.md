# Compiler heap footprint survey

Counts the `Tree`, `Position`, `Symbol`, `Type`, `Name` and `Scope` instances live after typer on a corpus, and models what candidate field changes would do to their total size. Findings: [FINDINGS.md](FINDINGS.md). Rendered page: `out/tree-footprint.html`.

The scripts use the published scalac 2.13.18 and JOL 0.17 through scala-cli. Run them from this directory.

1. `Survey.scala <corpus>` compiles a compiler-benchmark corpus with `-Ystop-after:typer`, keeps the `Run`, and prints two tables as TSV:
   - `W` rows: trees reached by an identity-deduplicated walk of the unit bodies, including `TypeTree.original`, and the attachments and positions they hold.
   - `H` rows: the live class histogram (`GC.class_histogram`, which runs a full GC first).
   - `S`, `F`, `A` and `N` rows: a reflective walk of everything reachable from `Global`, run after the histogram. For each `Symbol`, `Type`, `Name`, `Scope`, `ScopeEntry`, `BaseTypeSeq` and `TypeHistory` class it gives the instance count and, per field, how many instances hold something other than null, zero or a sentinel such as `NoSymbol`. It sums the arrays those objects (and the name table) own, per field. `-Dsurvey.nowalk` skips it.

   ```bash
   scala-cli run --server=false Survey.scala -- scala > out/scala.tsv
   scala-cli run --server=false --java-opt -XX:+UseCompactObjectHeaders --java-prop survey.nowalk=1 Survey.scala -- scala | grep '^H' > out/scala-coh.tsv
   ```

   The second run takes the histogram with compact headers, so the 8-byte model can be checked as well.

   Corpora are read from `-Dsurvey.corpora` (default `~/code/scala/compiler-benchmark/corpus`), as `<name>/latest`. The `scala` corpus needs scala-asm 5.1.0 on the classpath, which is looked up in `-Dsurvey.deps` (default: jvm-perf-lab's `labs/scala2/target/deps`).

2. `Layout.scala <corpus>...` reads `out/<corpus>.tsv` and `out/<corpus>-coh.tsv`. For every tree class it lists the fields with JOL, then computes the instance size for each scenario and header size. It prints the per-class table, the weighted totals and the position classes. The `#### objects` sections do the same for symbols, types, names and scopes, with their own candidates, and add field layouts (`L` rows) and field use (`U` rows). It flags any class whose modelled size differs from the histogram's bytes per instance, with either header.

   ```bash
   scala-cli run --server=false Layout.scala -- scalap re2s scala > out/layout.txt
   ```

3. `extract.py` turns `out/layout.txt` into `out/data.json`. `gen.py` fills `page.tpl` from that file and writes `out/tree-footprint.html`.

   ```bash
   python3 extract.py && python3 gen.py
   ```

Tree sizes are computed by arithmetic: the header (12 or 8 bytes) plus the field sizes, rounded up to 8. JOL's own `HotSpotLayouter` returned empty layouts for hand-built `ClassData`, and the run exited without output when JOL attached to the live VM to read its layout. With only 4-byte and 1-byte fields, the arithmetic gives the same sizes as HotSpot's field layout on JDK 15 and later.

Symbols carry a `long`, and several types carry 1-byte flags, so for them `Layout.scala` models HotSpot's placement: class by class from `Object` down, primitives largest first, then references, each into the smallest gap that fits it at its natural alignment, otherwise at the end. Both models match the measured size of every class with both header sizes.
