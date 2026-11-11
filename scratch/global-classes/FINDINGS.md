# Does a real sbt build reach 3+ Global subclasses in one class loader? Yes.

Branch `instrument-global-subclasses` (retronym/scala, off origin/2.13.x): `Global`'s constructor records `getClass` in a static set (per class loader), prints `[global-classes] new=… count=… loader=… thread=… all=[…]` on each new class, and throws `IllegalStateException` once the set exceeds `-Dscala.globalClasses.max` (default 2). Scratch project and logs are on that branch under `scratch/global-classes/`.

## Result

sbt 1.12.15, JDK 21, Scala `2.13.19-bin-SNAPSHOT` (publishLocal of the branch). Three subprojects: `macros` (a macro whose impl calls `c.eval`, as jsoniter-scala's `JsonCodecMaker` does), `app` and `app2` using it.

`sbt -Dscala.globalClasses.max=100 clean compile doc 'set app/Compile/console/scalacOptions += "-Xnojline"' app/console`:

```
[global-classes] new=scala.tools.xsbt.ZincCompiler count=1 loader=sbt.internal.classpath.ClassLoaderCache$Key$CachedClassLoader@2c07e25b thread=pool-12-thread-2 all=[scala.tools.xsbt.ZincCompiler]
[global-classes] new=scala.tools.reflect.ToolBoxFactory$ToolBoxImpl$ToolBoxGlobal count=2 loader=…@2c07e25b thread=pool-12-thread-11 all=[ToolBoxGlobal, ZincCompiler]
[global-classes] new=scala.tools.nsc.doc.DocFactory$compiler$ count=3 loader=…@2c07e25b thread=pool-13-thread-14 all=[DocFactory$compiler$, ToolBoxGlobal, ZincCompiler]
[global-classes] new=scala.tools.nsc.interpreter.IMain$$anon$1 count=4 loader=…@2c07e25b thread=pool-15-thread-2 all=[DocFactory$compiler$, IMain$$anon$1, ToolBoxGlobal, ZincCompiler]
```

With the default limit, `sbt clean compile doc` throws in `app2 / Compile / doc`:

```
[error] java.lang.IllegalStateException: More than 2 subclasses of Global in one class loader: [global-classes] new=scala.tools.nsc.doc.DocFactory$compiler$ count=3 loader=sbt.internal.classpath.ClassLoaderCache$Key$CachedClassLoader@4e011557 … all=[scala.tools.nsc.doc.DocFactory$compiler$, scala.tools.reflect.ToolBoxFactory$ToolBoxImpl$ToolBoxGlobal, scala.tools.xsbt.ZincCompiler]
```

Pathways, all in the same scala-compiler class loader:

| task | Global subclass |
|---|---|
| `compile` (any subproject, Compile or Test) | `scala.tools.xsbt.ZincCompiler` (from `scala2-sbt-bridge`) |
| macro calling `c.eval` / `c.untypecheck`+toolbox during `compile` | `ToolBoxFactory$ToolBoxImpl$ToolBoxGlobal` |
| `doc` (also run by `publishLocal`/`publish`) | `scala.tools.nsc.doc.DocFactory$compiler$` (a `ScaladocGlobal`) |
| `console` | `scala.tools.nsc.interpreter.IMain$$anon$1` (`Global with ReplGlobal`) |

So `compile; doc; console` alone reaches 3 without any macros, and a `c.eval` macro library (jsoniter-scala, etc.) plus `publishLocal` or `doc` reaches 3. With `console` it is 4, the same count compiler-lab measured as ~55% slower on re2s.

## Class loader sharing

- sbt loads scala-compiler through `sbt.internal.classpath.ClassLoaderCache`, keyed by the scala jars. All three subprojects (compiled in parallel on different threads), `doc`, and `console` got the same loader (`@2c07e25b` in one run).
- With `sbt --client` (server), separate commands `compile` then `doc` hit the same loader (`@3fecb366`), and after `reload` + `clean; compile` no new class was registered, i.e. the cached loader (and so the JIT profile pollution) survives `reload` for the life of the server.
- A different scalaVersion would get its own loader and its own count. The sbt meta-build (2.12) is separate.
- Not tested: Metals/Bloop (MetalsGlobal = `interactive.Global` subclass runs in the Metals JVM, alongside Bloop's bridge Global in Bloop's JVM), and runtime ToolBoxes in tests (loaded from the test classpath, normally a different loader from sbt's compiler loader).

## Synthetic check

`scratch/global-classes/ThreeGlobals.scala` on the quick build: three `new Global(_, _) {}` classes, throws at the third; with `-Dscala.globalClasses.max=5` all three compile.

## Aside

`console` with sbt 1.12.15 and the 2.13.19 snapshot fails with `NoSuchMethodError: org.jline.terminal.impl.MouseSupport.keys` (jline mismatch between sbt's jline and the REPL's); `-Xnojline` works around it.
