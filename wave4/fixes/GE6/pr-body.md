This pull request holds a failing test and no fix; how scalac should bound the analysis is a design choice, set out below.

Under the inliner, a method with a couple of hundred exception handlers fails to compile:

```scala
class K { def f(i: Int): Option[Int] = Some(i) }
object C {
  def run(k: K): String = {
    val sb = new StringBuilder
    try { val r: Int = k.f(1).map(x => x + 1).filter(_ > 0).map(_ * 2).getOrElse(0); sb.append(r) } catch { case e: Throwable => sb.append(e) }
    // ... the same line 200 times
    sb.toString
  }
}
```

`scalac -opt:inline:** -opt:local` reports "Error while emitting C$: Too many allocated bytes". The test generates the method. A method of this shape is a test or main method that wraps each of many checks in try/catch, or generated code; it is well under the JVM's method size limit, and 2.13.18 compiles it.

This is a regression in the unreleased 2.13.19 from the upgrade to ASM 9.11.1 (scala/scala#11327). ASM 9.11 counts the bytes of every `Frame` its `Analyzer` allocates during one analysis, garbage included, in an `int`, and throws `LimitExceededException` when the count passes the limit. scalac raises the limit to `Int.MaxValue` in `AsmAnalyzer` (and in `ClassWriterWithBTypeLub` for frame computation), but cannot raise it further. The exception here comes from the `ProdConsAnalyzer` that `CopyProp.eliminateStaleStoresAndRewriteSomeIntrinsics` requests. After inlining the method has thousands of instructions, hundreds of locals and one handler per line; the analysis revisits the rest of the method after each handler merge and allocates handler frames per covered instruction per visit, so the total passes 2 GiB although little is live at once. The size guard `AsmAnalyzer.sizeOKForSourceValue` estimates the cost as instructions times locals squared and does not account for handlers, so it lets this method through. In 2.13.18 the same analysis ran without a limit; the 220-line version took about a minute, so the guard was already too generous for handler-heavy methods.

Options:

- Catch `LimitExceededException` where the optimizer runs an analysis and skip that optimization for the method, as the `sizeOK*` guards skip methods they judge too large. The guards skip silently today, so there is no existing warning to reuse; the catch has to sit where no transformation of the method has started, since several of them request their analysis lazily part way through.
- Make the size estimate in `AsmAnalyzer` account for the number of exception handlers, so such methods are skipped up front, before the time is spent.
- Count in a `long`, or disable the counter, in the scala-asm fork; the analysis then succeeds but takes as long as in 2.13.18.

With either of the first two the test compiles quickly; with the third it takes about as long as 2.13.18 did.

Found in the fourth wave of the coordinated code review of 2.13.x (finding GE6). Prepared with Claude.
