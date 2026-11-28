# collections3: ordinary usage chosen by coverage, Range model, self-aliasing matrix

Third round of the collections harness. No dependencies beyond scala-library (JaCoCo only for measuring).

## ordinary/OrdDiff.scala, gen_ops.py, cov.py

Ordinary calls with in-range arguments on the files earlier rounds left unreached. `gen_ops.py` writes `SeqLikeOps.scala`: one list of about 110 sequence operations instantiated for Array (ArrayOps), String (StringOps) and collection.Seq (immutable and mutable ArraySeq), each compared with the same call on a List. `OrdDiff.scala` has the families: `seq` (that list over ten element types, plus LazyZip, ArrayBuilder, steppers, streams, StringBuilder against java.lang.StringBuilder), `parse` (StringParsers against java.lang parsing), `vector` (programs of calls against an Array[Int] model, sizes around 32^n), `hash` (immutable HashMap/HashSet bulk operations with colliding keys against TreeMap/TreeSet).

Run: `python3 gen_ops.py; scalac -d out OrdDiff.scala SeqLikeOps.scala; java -javaagent:<jacoco agent>=destfile=x.exec,includes=scala.* -cp scala-library.jar:out OrdDiff <seq|parse|vector|hash|all> <seed> <seconds>`, then `java -jar <jacoco cli> report x.exec --classfiles scala-library.jar --xml x.xml; python3 cov.py x.xml`. Measured per minute under the agent: about 18,000 seq cases (2.2 million checks), 6 million parsed strings, 6,500 vector programs, 550 hash cases.

## RangeDiff.scala

Generates a `Range` or a `NumericRange` of Int, Long, Short, Byte, Char, BigInt or BigDecimal (scale 0, 2, 5) with start, end and step drawn from the type's extremes, powers of two, small values and steps that divide the span into a few elements; inclusive and exclusive; both signs. Applies zero to two of take, drop, takeRight, dropRight, slice, splitAt, tail, init, reverse with Int arguments at the extremes, then runs about 50 queries.

Oracle: a model `(start, end, step, inclusive)` in BigInt (BigDecimal values are scaled integers) and a window `(first index, count, reversed)` on it. Nothing is materialised unless the range has at most 300 elements; then the range is also compared with the `List` of its elements (equality both ways, hashCode, takeWhile/span, grouped, sliding, tails, sorted, patch and so on). A range of more than Int.MaxValue elements may answer correctly or throw IllegalArgumentException. Iterators are drained by hand, because `toList` and `mkString` consult `knownSize`, which is itself checked.

Failures are grouped by signature (type, operations, query, kind of expected and actual result) with up to three examples each. A watchdog reports the query that gave no result after 8 s and stops the run. `quarantine=1` leaves out the inputs that reproduce the findings already recorded (empty Range iterators with drop, iterator knownSize, a step of Int.MinValue) so that a run gets past them.

Run: `scalac -d out RangeDiff.scala; java -cp scala-library.jar:out RangeDiff <seed> <seconds> [quarantine=1]`. Measured: 482,467 ranges and 23.9 million checks in 60 s.

## AliasMatrix.scala, matrix.py

For every mutable collection class and every bulk method, feeds the method a source derived from the receiver: the collection, its view, mapped, filtered, sliced and reversed views, its iterator, a LazyList of it, keySet/values derivatives for maps; plus in-place methods whose function reads the receiver, and `getOrElseUpdate`/`updateWith` whose function writes to the map. 1,312 cells, each with 3 and with 40 elements.

Oracle: the argument is read as it was before the call. The expected state is the same call with the source derived from a second, equal instance that the call does not modify; for the re-entrant map cells it is the two steps done one after the other. Verdict per cell: ok, CME, WRONG (returns another state), an exception name, HANG (2 s, or heap exhausted). The driver exits with status 3 after a HANG and `run.sh` restarts it at the next cell. `matrix.py` renders the TSV as one table per method. `alias-integration-build.tsv` is the output on the integration build, with expected and actual state for every cell that is not ok.

The in-place rows with a function that reads the receiver (`mapInPlace`, `filterInPlace`, `mapValuesInPlace`) are WRONG by this model for most classes; that is the model being strict about something the documentation leaves open, not a defect list.

Run everything: `run.sh <build/pack> <seconds> [seed]` (`QUARANTINE=1` for the Range quarantine, `JACOCO_AGENT=<agent jar>` to record coverage of the ordinary-usage run, `ALIAS=1` to add the matrix, which takes about 4 minutes, most of it waiting for the cells that do not terminate).
