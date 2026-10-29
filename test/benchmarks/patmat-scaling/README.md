# Pattern match analysis scaling harness

This is the harness behind the measurements in retronym/scala#142 and the page at https://claude.ai/artifact/UJib6qLN3Edjd6DtXh9bit. It is kept here for the record and is not wired into the build. For a version that runs from sbt against the compiler in this checkout, see `PatmatScaling` and `PatmatAnalysisBenchmark` in `test/benchmarks/src/main/scala/scala/tools/nsc/transform/patmat`.

What this version adds over the sbt one:

- It drives Scala 3 as well as Scala 2 from the same generated sources, which is how the page compares 2.13.x, #142, Scala 3 and retronym/scala3#23. The phase time comes from `-Vstatistics:patmat` for Scala 2 and from `-Yprofile-enabled` (the MegaPhase containing `patternMatcher`) for Scala 3.
- Each compiler runs from a plain classpath, so any build can be measured: a release, a branch, or each commit of a series.
- A hard timeout per compile. A compile can't be interrupted, so `Bench` records the timeout and exits with code 3, and `run.sh` continues with the remaining shapes in a fresh JVM.
- `fit.py` fits the exponents and `build_page.py` renders the page from `results/all.csv`, including the per-commit evolution of #142 (`steps.tsv` names the labels `c01`..`c11`).

## Files

- `Gen.java`: writes `srcs/<shape><N>.scala`. The shapes are the same as `PatmatShapes`.
- `Bench.java`: warms up on N=64, then for each shape doubles N, taking the minimum phase time over a few compiles. It stops a shape early when the next size is predicted to take over 90 s.
- `run.sh <label> <s2|s3> <classpath> [JVM options]`: one compiler's sweep, appended to `results/all.csv`.
- `fit.py`, `build_page.py`, `page.template.html`, `steps.tsv`: the analysis and the page.
- `results/all.csv`: the data behind the page as published.

## Example

```
./run.sh scala2-base s2 $LIB:$REFLECT:$COMPILER:$ASM
./run.sh scala3-patch s3 $SCALA3_CP "-Ddotty.patmatConstantSkip=true -Ddotty.patmatHashMemo=true"
python3 build_page.py .
```

Above about 2000 cases Scala 2 gives up the analysis ("Cannot check match", `AnalysisBudget.maxFormulaSize`), so `fit.py` leaves Scala 2's N=2048 points out of the fits and the page does not plot them.
