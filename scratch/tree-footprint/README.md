# Tree and Position footprint survey

Counts the `Tree` and `Position` instances live after typer on a corpus, and models what candidate field changes to `Tree` would do to their total size. Findings: [FINDINGS.md](FINDINGS.md). Rendered page: `out/tree-footprint.html`.

The scripts use the published scalac 2.13.18 and JOL 0.17 through scala-cli. Run them from this directory.

1. `Survey.scala <corpus>` compiles a compiler-benchmark corpus with `-Ystop-after:typer`, keeps the `Run`, and prints two tables as TSV:
   - `W` rows: trees reached by an identity-deduplicated walk of the unit bodies, including `TypeTree.original`, and the attachments and positions they hold.
   - `H` rows: the live class histogram (`GC.class_histogram`, which runs a full GC first).

   ```bash
   scala-cli run --server=false Survey.scala -- scala > out/scala.tsv
   ```

   Corpora are read from `-Dsurvey.corpora` (default `~/code/scala/compiler-benchmark/corpus`), as `<name>/latest`. The `scala` corpus needs scala-asm 5.1.0 on the classpath, which is looked up in `-Dsurvey.deps` (default: jvm-perf-lab's `labs/scala2/target/deps`).

2. `Layout.scala <corpus>...` reads `out/<corpus>.tsv`. For every tree class it lists the fields with JOL, then computes the instance size for each scenario and header size. It prints the per-class table, the weighted totals and the position classes. It flags any class whose modelled 12-byte size differs from the histogram's bytes per instance.

   ```bash
   scala-cli run --server=false Layout.scala -- scalap re2s scala > out/layout.txt
   ```

3. `extract.py` turns `out/layout.txt` into `out/data.json`. `gen.py` fills `page.tpl` from that file and writes `out/tree-footprint.html`.

   ```bash
   python3 extract.py && python3 gen.py
   ```

The sizes are computed by arithmetic: the header (12 or 8 bytes) plus the field sizes, rounded up to 8. JOL's own `HotSpotLayouter` returned empty layouts for hand-built `ClassData`, and the run exited without output when JOL attached to the live VM to read its layout. With only 4-byte and 1-byte fields, the arithmetic gives the same sizes as HotSpot's field layout on JDK 15 and later. With the 12-byte header it matches the measured size of every tree class.
