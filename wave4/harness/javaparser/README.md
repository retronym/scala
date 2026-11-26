# javaparser: scalac's Java source parser against javac

`gen.py <out> <cases> <seed> [features]` writes, per case, a package of Java sources (JDK 21 level) and a `Probe.scala` that uses every public member. Features: `K` constant fields of every primitive type and String with generated constant expressions (literal forms, casts, shifts, ternaries with mixed types, string concatenation, text blocks, unicode and octal escapes, qualified forward references, non-constant initialisers); `E` enums; `R` records; `S` sealed hierarchies; `An` annotations with every element type; `G` generics; `I` interfaces and a SAM; `B` initialisers and bodies the parser must skip, each followed by a sentinel member; `M` modifier orders, array and varargs forms, throws. Comments and annotations are scattered between modifiers.

`Driver.scala` runs all cases in one JVM, with a fresh `Global` per compilation and a 60 s timeout per case:

1. javac in process (`-parameters`); a reject is counted and printed as `JAVAC-REJECT`.
2. Symbol oracle (`SYM` lines): the non-private members of every class as scalac types them from the `.java` sources, against the same members read from javac's class files by scalac's class file parser. Compared per member: flags, access boundary, annotations, type at typer, type after erasure, constant value, sealed children.
3. Probe oracle (`PROBE` lines): (A) scalac on `Probe.scala` and the Java sources together, (B) scalac on `Probe.scala` against the class files; both run against javac's classes. Compared: compile success, diagnostics, printed output.

Run: `OUT=<dir> ./run.sh <build/pack> <cases> <seed> [features]`; the log is `<dir>/log.txt`, sources under `<dir>/cases/<pkg>`. Group lines with `sed 's/p0[0-9]*/pN/g' log.txt | sort | uniq -c`. Measured: 300 cases in 92 s (about 195 cases a minute, each with 5 compilations).

Symbol-oracle lines that are not defects and are still printed: records from class files have explicit `equals`, `hashCode`, `toString`; `T[]` for a bounded `T` prints as `Array[T with Object]` from source (same erasure). `@transient` on varargs methods from class files (ACC_VARARGS shares the bit) and `strictfp` are filtered. javac does not emit `ConstantValue` for `long >>> long`, so the generator keeps long shift counts of flat chains to `int`.

Findings from this harness are `wave4/findings/GJ1` to `GJ4`; their `cmd.sh` shows the two compilation orders for a hand-written reproduction.
