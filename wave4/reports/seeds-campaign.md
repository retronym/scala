# Seeds campaign (prefix WS)

Build under test: w4-int for everything below except round 1, which also ran on w3-int with the same result (w4-int exists from the start of the hunt). Corpus: 944 single-file run and jvm tests with a `.check` file; 269 excluded (`harness/seeds/excluded.tsv`: 251 extend partest classes, 6 `javaOpt`, 4 multi-file, 4 `filter`, 2 `jvm`, 1 exit, 1 stdin). Five baseline runs do not compile for lack of junit and the testkit (BoxUnboxTest, productElementName, small-seq-apply, scalapInvokedynamic, scala-concurrent-tck) and are skipped in every comparison; the other 939 are the oracle's population.

## Rounds

| Round | Added | Outcomes compared | Differences | After classification |
|---|---|---|---|---|
| 1 | `-opt:local`, `-Ydelambdafy:inline` | 2 x 939 | 12 | 1 finding (WS1); the rest are lambda and local class names, reflection of own bytecode, JIT-elided exception messages, a test that asserts the lambda form |
| 2 | `@specialized` removed (24 seeds change), separate compilation in given order and reversed (366 seeds have two or more units) | 24 + 2 x 366 | 8 + 58 + 346 | none: nospec differences are specialized method names and counts (reflection); separate compilation errors are dependencies on later units, sealed hierarchies split across files, file names in messages; reversed order fails by construction |
| 3 | dependency-ordered separate compilation, `-Xsource:3`, 2.13.18 | 366 + 2 x 939 | 29 + 1 + 11 | none: 337 of 366 compile and agree; the rest are cycles and sealed hierarchies. `-Xsource:3` rejects one test (run/collections). All 11 differences against 2.13.18 are 2.13.18 doing worse (two compile errors, VerifyError in copy-cycle-producers, a timeout in seqlike-kmp, BootstrapMethodError) or ordering and descriptions in reflection output; no case where 2.13.18 matches the `.check` and the build does not |
| 4 | `-opt:inline:** -opt:local`, `-Ycheck:all`, `-opt:local` with `-Ydelambdafy:inline` | 3 x 939 | 14 | 1 finding (WS2); run/t10277 prints fewer specialized method names when inlined; two stack-message and numbering differences |

Cases are seed times configuration outcomes. Round 1 ran twice (w3-int, then w4-int with JaCoCo) with identical differences.

## Findings

- WS1 (area_code VCL): `-Ydelambdafy:inline` rejects `def g = () => f` when `f` returns a local trait/abstract class/trait chain typed as an existential (run/existentials3-new and -old). The type mismatch comes from the typer run inside UnCurry. Same on base and 2.13.18.
- WS2 (area_code T): `-Ycheck:typer` does not terminate on `if (b) LazyList.empty else Nil`; run/t498 with `-Ycheck:all` hangs at uncurry on w3-int and w4-int, where the base completes (base hangs with `-Ycheck:uncurry` alone). Needs a second look: one thread dump, cause not isolated, debug option only.

Differences in program results (as opposed to compile errors and reflection of the program's own class names): none. No `-opt:local`, `-opt:inline:**`, delambdafy or separate-compilation run changed the printed output of a test whose output does not depend on bytecode shape.

## Coverage (JaCoCo on w4-int, cumulative per round, lines covered of total)

| Scope | Base only | Round 1 | Round 2 | Round 3 | Round 4 |
|---|---|---|---|---|---|
| `scala/tools/nsc/transform` | 6712 / 8729 | 6714 | 6714 | 6714 | 6715 |
| `scala/tools/nsc/backend/jvm` | 6355 / 8225 | 6567 | 6594 | 6594 | 6701 |
| `scala/tools/nsc/typechecker` | 9338 / 14385 | 9361 | 9589 | 9591 | 9908 |

The `Base only` column is the reference panel for the other tracks (`reports/seeds-treemap.html`, `reports/seeds-backend-treemap.html`, panels baseline and rounds 1 to 4). Round 3's `-Xsource:3` and 2.13.18 runs were not instrumented. The corpus under the baseline reaches 77 percent of transform and 77 percent of backend/jvm lines. Largest unreached methods in transform and backend/jvm throughout: `AnfTransform$AnfTransformer.$anonfun$transform$1`, `ExprBuilder$AsyncBlockBuilder.add`, `AliasingFrame.execute`, `AsyncPhase$AsyncTransformer$UseFields.transform`, `ProdConsAnalyzer.copyOperationProducedValueSlots` (async and optimizer internals).

## Verdict

Transform coverage did not move in four rounds; the rewrites change which code paths of the back end run (+346 lines from the optimizer configurations) but not which transforms. The seed corpus is exhausted for these rewrites. The two findings came from the delambdafy and check configurations, not from the rewrites of the brief. Stop rule: rounds 2 and 3 had no new signature and no transform gain; round 4 found WS2 and added typechecker lines, then stopped. A further campaign would need `-Xasync` seeds, the partest-class tests (251, need the partest library on the class path) and junit tests (testkit on the class path), which are the unreached async and optimizer code.
