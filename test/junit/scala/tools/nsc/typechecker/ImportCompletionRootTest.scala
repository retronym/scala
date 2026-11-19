package scala.tools.nsc.typechecker

import org.junit.Assert.assertSame
import org.junit.Test

import scala.tools.testkit.BytecodeTesting

class ImportCompletionRootTest {
  /** Its completion types the qualifier and so waits for source symbols: under the coarse root that closes a false cycle. */
  @Test def anImportAtTheTopOfAFileHasItsOwnRoot(): Unit = {
    val compiler = BytecodeTesting.newCompiler(extraArgs = "-Yparallel-typer -Yparallel-typer-threads 2")
    import compiler.global._
    new Run
    val imp = rootMirror.EmptyPackageClass.newImport(NoPosition)
    assertSame(imp, completionRoot(imp))
  }
}
