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

package scala
package reflect
package internal

import Flags._

/** Undoes, when a run starts, the writes that the last run made to the names and flags of symbols loaded from the
 *  class path, so that a `Global` kept across runs sees what a fresh one would.
 *
 *  A loaded symbol comes from a pickle taken before lowering. When a source class extends a loaded trait, the info
 *  transformers replay the trait's lowering on its symbols: `fields` and `mixin` expand the names of private
 *  accessors and objects and make them not private, `uncurry` and `fields` turn nested objects into methods, and
 *  `fields`, `flatten` and the typer leave bits behind (`RewoundFlags`). Infos have a history that a new run discards,
 *  but names and raw flags do not, so these writes used to carry into every later run: a member expanded in the last
 *  run is missed by the typer's lookup by its source name, and a field is named after the expanded name of its object.
 *
 *  The journal records a symbol's name and flags before the first such write in a run. It must, because
 *  `EXPANDEDNAME` is pickled: a name expanded in the last run cannot be told from one that was loaded expanded.
 */
trait Rewinds { self: SymbolTable =>

  private[this] var completionDepth = 0

  /** Bracket a completer, so that a symbol knows whether a loader made it (`Symbol.isCreatedInCompletion`). */
  @inline private[internal] final def enteringCompletion(): Unit = completionDepth += 1
  @inline private[internal] final def exitingCompletion(): Unit = completionDepth -= 1
  private[internal] final def isInCompletion: Boolean = completionDepth > 0

  /** Is the class path the source of truth for `sym`'s name and flags? It was made while a completer ran, it has
   *  finished completing itself, and its top-level class was read from a class file. Reads fields only: asking must
   *  not complete or adapt anything, because it is asked from the flag setters.
   *
   *  A check that loaded symbols are not written at all (the "freeze check") would ask the same question.
   */
  def isLoadedFromClassfile(sym: Symbol): Boolean =
    sym.isCreatedInCompletion && sym.isInitialized && !sym.hasPackageFlag && {
      val f = sym.enclosingTopLevelClass.associatedFile
      (f ne null) && (f.hasExtension("class") || f.hasExtension("sig") || f.hasExtension("tasty"))
    }

  private final class RewindEntry(val sym: Symbol, val name: Name, val flags: Long, val mask: Long, val restoreName: Boolean)

  private[this] val rewindSeen = new java.util.IdentityHashMap[Symbol, Symbol]
  private[this] val rewindLog = new java.util.ArrayList[RewindEntry]

  /** The number of symbols journaled in this run so far. */
  final def rewindJournalSize: Int = rewindLog.size

  /** Called by the setters of the name and of `RewoundFlags`, before the write, when it changes something.
   *
   *  A non-static object made a method by `uncurry` or `fields` is journaled whatever its origin, and only those two
   *  bits are restored: this replaces the undo that `adaptToNewRunMap` used to make on every object in the
   *  declarations of a class, which also covered objects compiled from source in an earlier run (the REPL).
   */
  private[internal] final def noteRewoundWrite(sym: Symbol, changed: Long): Unit =
    if (isCompilerUniverse && !rewindSeen.containsKey(sym)) {
      val entry =
        if (isLoadedFromClassfile(sym)) new RewindEntry(sym, sym.rawname, sym.rawflags, RewoundFlags, restoreName = true)
        else if ((changed & (METHOD | STABLE)) != 0L && (sym.rawflags & MODULE) != 0L) new RewindEntry(sym, sym.rawname, sym.rawflags, METHOD | STABLE, restoreName = false)
        else null
      if (entry ne null) {
        rewindSeen.put(sym, sym)
        rewindLog.add(entry)
      }
    }

  /** Restore the journaled names and flags, newest first, and empty the journal. Called when a run starts.
   *
   *  This is eager, not part of `adaptToNewRunMap`, which adapts a symbol's info the first time it is read in the new
   *  run: a lookup by name goes through the owner's declarations before the info of any member is adapted, so a name
   *  restored lazily would be restored too late for the lookup that should find it.
   *
   *  The name is restored through `name_=`, which rehashes the symbol in the declarations of every info of its owner.
   */
  final def rewindForNewRun(): Unit = {
    var i = rewindLog.size - 1
    while (i >= 0) {
      val e = rewindLog.get(i)
      val sym = e.sym
      if (e.restoreName && (sym.rawname ne e.name)) sym.name = e.name
      sym.rawflags = (sym.rawflags & ~e.mask) | (e.flags & e.mask)
      i -= 1
    }
    rewindLog.clear()
    rewindSeen.clear()
  }
}
