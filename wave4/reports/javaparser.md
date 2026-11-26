# javaparser

## What was built

`wave4/harness/javaparser/`: `gen.py` generates a package of Java sources (JDK 21 level) plus a Scala probe per case; `Driver.scala` runs every case in one JVM against two oracles, with javac as the reference. The symbol oracle compares each non-private member as scalac types it from the `.java` source with the same member read from javac's class file through scalac's class file parser (flags, access boundary, annotations, type at typer, erased type, constant value, sealed children). The probe oracle compiles `Probe.scala` jointly with the Java sources and separately against the class files, runs both against javac's classes, and compares compile success, diagnostics and output. Coverage was not measured.

## Numbers

- Runs on the integration build, while the oracle was being corrected: 10, 20, 20, 20, 40, 150 and 300 cases. The last: 300 cases in 92 s, 0 javac rejects, 0 timeouts, 67 clean, 233 with at least one line.
- javac rejects were 5 of 10 in the first run and 1 of 20 in each of the next three, all generator faults (a `;` between `package` and `import`, an illegal forward reference in a nested class, a field named `K2` hiding class `K2`); 0 of 490 after that.
- False alarms, all the oracle's fault and fixed or filtered: the dump walked the flat `G$Inner` package-level symbol and hit an assertion in the class file parser; FINAL is set on fields only by the class file parser; static-side and interface constructors exist only from source; sealed children are registered lazily (needs a completing pass); `throws` annotations print their argument differently; `@transient` appears on varargs methods from class files (ACC_VARARGS shares the bit); `Objects.hash` of an array in a probe; proxy class names of annotations.
- Remaining printed lines that are not defects: record `equals`/`hashCode`/`toString` are explicit only in class files (172 of 300 cases); `T[]` with bounded `T` prints `Array[T with Object]` from source with the same erasure (74).
- Signatures left after triage: 5 (four findings below, one known).
- Constant folding: no value or type mismatch between scalac's folding of Java source constants and javac's `ConstantValue` in any run. One difference in the other direction: javac emits no `ConstantValue` for `long >>> long` (for example `4L >>> 0777L`), while scalac folds it as JLS 15.29 says; the printed values agree. The generator now avoids it.

## Findings

In `wave4/findings/`; each `cmd.sh` does the joint and the separate compilation and prints both.

- GJ1: a sealed Java interface without `permits`, implemented by records in the same file, has no known subclasses in joint compilation: spurious "unreachable code", no exhaustivity warning. Integration and base; absent on 2.13.18 (regression); absent on Scala 3.7.4.
- GJ2: the accessor of a varargs record component is typed `Int*` from source, so `List(r.rest)` is rejected jointly and compiles against the class file. All three builds; Scala 3.7.4 accepts.
- GJ3: a Java annotation whose argument is a constant expression (`@Deprecated(since = "1" + "2")`) is dropped from source, so the deprecation warning is missing in joint compilation. All three builds; related to B9 and J5.
- GJ4: `enum E { , }` and `enum E { ,; ... }` are rejected by the Java parser; javac accepts. All three builds.

Under-specified or not scalac's fault, not recorded: javac emits no `ConstantValue` for `long >>> long` while scalac folds it (values agree).

Known and seen again: `T extends Comparable<? super T> & Serializable` gives "illegal cyclic reference involving type T" in joint compilation (X22, scala/bug 4744); removed from the generator after 7 of 40 cases hit it.

Not pursued: the assertion `class Inner: class G$Inner` in `ClassfileParser.MemberTypeCompleter` when the flat symbol `G$Inner` of a non-static inner class with its own inner class is completed from the package scope. The driver triggered it by walking package members; whether Scala source can reach it (a backquoted `` `G$Inner` `` reference) was not tried.

## What the generator does not yet produce

Local and anonymous classes outside method bodies, static imports that are used, `package-info.java`, module declarations, type annotations (`int @A [] x`, `java.lang.@A String`), receiver parameters with annotations, javadoc `@deprecated` tags, unicode escapes outside literals, non-ASCII identifiers, nested generic inner types in signatures beyond one level, interfaces with generic default methods overridden from Scala, negative probes (Scala code that both orders must reject), package-private and protected access from another package, several top-level types in one file, and Java sources that refer to the Scala probe's classes. Shrinking is manual. The record and bounded-array print differences should be normalised before a longer run.

## Longer campaign

Yes, a moderate one. The constant folder held up, so the yield is in the typing of newer Java constructs from source (records, sealed types, annotations with expression arguments): all four findings appeared within the first 70 cases, and the 450 cases after that added no signature, so more cases from this generator will not help; new generator features will. The cost is low at about 195 cases a minute. The next step is negative probes and cross-package access, which the JVM-access family from earlier waves suggests is productive, and type annotations, which the parser handles in few places.
