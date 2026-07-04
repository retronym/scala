package scala.reflect.internal.ijmodel

/**
 * Research knob (no IntelliJ analog): which guard semantics the engine runs under.
 *
 *   - prodGuard: `hasRecursiveThisType0` pre-scan (exact this containment OR
 *     rewrittenClass.isSubClass(containedThisClass)) — the incumbent.
 *   - progress:  postcondition — refuse a this-rewrite whose OUTPUT is a
 *     this-rooted PATH still rooted (via inheritance) in the class being
 *     rewritten; bare this-type outputs (leaf→leaf narrowings) always admitted.
 *   - consumed:  first-match-wins per this-class within one chain application;
 *     a match reached only through the escape-climb (scalac's unmatched case)
 *     does NOT consume.
 *
 * The three modes that matter: Unguarded shows the disease, Production is the
 * incumbent, Progress+Consumed is the candidate.  (Progress-only is a component,
 * not a candidate — without consumption it over-narrows redundant chains, the
 * SCL-7008 shape.)
 */
case class EngineMode(prodGuard: Boolean, progress: Boolean, consumed: Boolean) {
  override def toString: String =
    if (prodGuard) "Production"
    else if (progress && consumed) "Progress+Consumed"
    else if (progress) "Progress"
    else if (consumed) "Consumed"
    else "Unguarded"
}

object EngineMode {
  val Production       = EngineMode(prodGuard = true,  progress = false, consumed = false)
  val Unguarded        = EngineMode(prodGuard = false, progress = false, consumed = false)
  val ProgressConsumed = EngineMode(prodGuard = false, progress = true,  consumed = true)
  val allModes: List[EngineMode] = List(Production, Unguarded, ProgressConsumed)
}
