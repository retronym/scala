package io.github.retronym.sharing

/** Whether the state that [[Sharing]]'s operations guard is shared between threads.
 *
 *  The owner of the state chooses a mode when it is created and never changes it. The operations take the mode as a
 *  `given` (in Scala 2, an implicit), which the owner supplies from a `final` field:
 *
 *  {{{
 *  class Universe(shared: Boolean):
 *    given Mode = if shared then Mode.Concurrent else Mode.Batch
 *  }}}
 *
 *  - [[Mode.Batch]]: one thread. Every operation is the plain code it wraps, with no locking, no claims and no
 *    allocation, so a program that never shares state pays nothing for being written against this library.
 *  - [[Mode.Concurrent]]: several threads. Operations take monitors or ask the [[ClaimRegistry]] for claims.
 *
 *  How concurrent threads take a claim (serialised under one coarse lock, or in parallel with a claim per key) is the
 *  registry's choice, not the mode's: the operations test only [[concurrent]].
 *
 *  ### Representation
 *
 *  A value class over a `Boolean`, so that the test on a hot path is a field read and a branch, the code a plain
 *  `Boolean` field gives. An opaque type would add a load of its package object (the inliner's proxy for the opaque
 *  scope), and testing an `Int` would materialise a `Boolean`.
 *
 *  @param concurrent `true` if the state is shared between threads
 */
final class Mode(val concurrent: Boolean) extends AnyVal {
  override def toString: String = if (concurrent) "Mode.Concurrent" else "Mode.Batch"
}

object Mode {
  /** One thread: operations run their code as is. */
  val Batch: Mode = new Mode(false)
  /** Several threads: operations lock or claim. */
  val Concurrent: Mode = new Mode(true)
}
