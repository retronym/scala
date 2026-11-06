/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc
package typechecker

import scala.collection.mutable
import scala.reflect.internal.util.JavaClearable

/** Defines the sub-components for the namer, packageobjects, and typer phases.
 */
trait Analyzer extends AnyRef
            with Contexts
            with Namers
            with Typers
            with Infer
            with Implicits
            with EtaExpansion
            with SyntheticMethods
            with Unapplies
            with Macros
            with NamesDefaults
            with TypeDiagnostics
            with ContextErrors
            with StdAttachments
            with MacroAnnotationAttachments
            with AnalyzerPlugins
            with ImportTracking
            with ParallelTyper
{
  val global : Global
  import global._

  object namerFactory extends {
    val global: Analyzer.this.global.type = Analyzer.this.global
  } with SubComponent {
    val phaseName = "namer"
    val runsAfter = List[String]("parser")
    val runsRightAfter = None
    def newPhase(_prev: Phase): StdPhase = new StdPhase(_prev) {
      override val checkable = false
      override def keepsTypeParams = false

      def apply(unit: CompilationUnit): Unit = newNamer(rootContext(unit)).enterSym(unit.body)
    }

    /** Load the packages that `unit` names, as `Namer.enterPackage` will: the same lookups in the same order, at the
     *  namer's phase. Most of the namer's time is here (a package loader lists its class path and source path
     *  entries and enters a symbol for each), and none of it needs the other units, so the parallel parser runs it
     *  on the driver thread, unit by unit in the order of the units, while the workers parse. Nothing is created: a
     *  package that does not exist yet is left for the namer, and nothing below it can exist.
     */
    def loadPackagesOf(unit: CompilationUnit): Unit = {
      def existingPackage(owner: Symbol, pid: RefTree): Symbol = {
        val pkgOwner = pid match {
          case Ident(_)                 => if (owner.isEmptyPackageClass) rootMirror.RootClass else owner
          case Select(qual: RefTree, _) => existingPackage(owner, qual).moduleClass
          case _                        => NoSymbol
        }
        if (pkgOwner eq NoSymbol) NoSymbol
        else {
          val existing = pkgOwner.info.decls.lookup(pid.name)
          if (existing.hasPackageFlag && pkgOwner == existing.owner) existing else NoSymbol
        }
      }
      def load(owner: Symbol, tree: Tree): Unit = tree match {
        case PackageDef(pid, stats) =>
          val pkg = existingPackage(owner, pid)
          if (pkg ne NoSymbol) {
            pkg.info.decls
            stats.foreach(load(pkg.moduleClass, _))
          }
        case DocDef(_, defn) => load(owner, defn)
        case _               =>
      }
      enteringPhase(currentRun.namerPhase)(load(rootMirror.RootClass, unit.body))
    }
  }

  object packageObjects extends {
    val global: Analyzer.this.global.type = Analyzer.this.global
  } with SubComponent {
    val deferredOpen: mutable.Set[Symbol] = {
      import scala.jdk.CollectionConverters._
      // This will throw a ConcurrentModificationException if we mutate during iteration
      val javaSet = new java.util.LinkedHashSet[Symbol]()
      perRunCaches.recordCache(JavaClearable.forCollection(javaSet))
      javaSet.asScala
    }
    val phaseName = "packageobjects"
    val runsAfter = List[String]()
    val runsRightAfter= Some("namer")

    def newPhase(_prev: Phase): StdPhase = new StdPhase(_prev) {
      override val checkable = false
      import global._

      val openPackageObjectsTraverser = new InternalTraverser {
        override def traverse(tree: Tree): Unit = tree match {
          case ModuleDef(_, _, _) =>
            val sym = tree.symbol
            if (sym.name == nme.PACKAGEkw) {
              val owner = sym.owner
              deferredOpen.subtractOne(owner) // we've actually got a source file
              openPackageModule(sym, owner)
            }
          case ClassDef(_, _, _, _) => () // make it fast
          case _ => tree.traverse(this)
        }
      }

      def apply(unit: CompilationUnit): Unit = {
        openPackageObjectsTraverser(unit.body)
      }

      override def run(): Unit = {
        super.run()

        // openPackageModule can remove entries from `deferredOpen`, so make a defensive copy first,
        // and then check whether `remove` says the sym is still deferred.
        // `force` the open because we might be called earlier if the run is bailing on an early error.
        for (sym <- deferredOpen.toVector)
          if (deferredOpen.remove(sym))
            openPackageModule(sym, force = true)
      }
    }
  }

  object typerFactory extends {
    val global: Analyzer.this.global.type = Analyzer.this.global
  } with SubComponent {
    import global.statistics
    val phaseName = "typer"
    val runsAfter = List[String]()
    val runsRightAfter = Some("packageobjects")
    def newPhase(prev: Phase): StdPhase = new TyperPhase(prev)
    final class TyperPhase(prev: Phase) extends StdPhase(prev) {
      override def keepsTypeParams = false
      override def shouldSkipThisPhaseForJava: Boolean = !settings.YpickleJava.value && !createJavadoc
      resetTyper()
      // the log accumulates entries over time, even though it should not (Adriaan, Martin said so).
      // Lacking a better fix, we clear it here (before the phase is created, meaning for each
      // compiler run). This is good enough for the resident compiler, which was the most affected.
      undoLog.clear()
      override def run(): Unit = {
        val start = if (settings.areStatisticsEnabled) statistics.startTimer(statistics.typerNanos) else null
        global.echoPhaseSummary(this)
        val units = currentRun.units

        if (settings.YparallelTyper.value) typeCheckInParallel(this, units.toList, applyPhase(_))
        else
        while (units.hasNext) {
          applyPhase(units.next())
          undoLog.clear()
        }
        finishComputeParamAlias()
        // defensive measure in case the bookkeeping in deferred macro expansion is buggy
        clearDelayed()
        if (settings.areStatisticsEnabled) statistics.stopTimer(statistics.typerNanos, start)
      }
      def apply(unit: CompilationUnit): Unit = {
        try {
          val typer = newTyper(rootContext(unit))
          unit.body = typer.typed(unit.body)
          // interactive typed may finish by throwing a `TyperResult`
          if (!settings.Youtline.value) {
            // under the deque's monitor, as `addPostUnitCheck`: a completion of one of this unit's symbols on another thread adds to it
            def nextToCheck(): CompilationUnit.ToCheck = unit.toCheck.synchronized(if (unit.toCheck.nonEmpty) unit.toCheck.removeHead() else null)
            var check = nextToCheck()
            while (check ne null) { check.apply(); check = nextToCheck() }
            if (!settings.isScaladoc && settings.warnUnusedImport)
              warnUnusedImports(unit)
            if (!settings.isScaladoc && settings.warnUnused.isSetByUser)
              new checkUnused(typer).apply(unit)
          }
        }
        finally {
          runReporting.reportSuspendedMessages(unit)
          unit.toCheck.clearAndShrink()
        }
      }
    }
  }
}
