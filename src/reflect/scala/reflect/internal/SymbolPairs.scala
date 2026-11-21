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

import util.StringContextStripMarginOps
import scala.annotation.tailrec

/** An abstraction for considering symbol pairs.
 *  One of the greatest sources of compiler bugs is that symbols can
 *  trivially lose their prefixes and turn into some completely different
 *  type with the smallest of errors. It is the exception not the rule
 *  that type comparisons are done correctly.
 *
 *  This offers a small step toward coherence with two abstractions
 *  which come up over and over again:
 *
 *    RelativeTo: operations relative to a prefix
 *    SymbolPair: two symbols being related somehow, plus the class
 *       in which the relation is being performed
 *
 *  This is only a start, but it is a start.
 */
abstract class SymbolPairs {
  val global: SymbolTable
  import global._

  final case class SymbolPair(base: Symbol, low: Symbol, high: Symbol) {
    private[this] val self  = base.thisType

    // The member types of `low` and `high` that the cursor computed to match them, if any; see `knownMemberType`.
    private[this] var knownLow: MemberTypeOf = null
    private[this] var knownHigh: MemberTypeOf = null
    private[SymbolPairs] def seed(low: MemberTypeOf, high: MemberTypeOf): this.type = {
      knownLow = low
      knownHigh = high
      this
    }

    def pos                 = if (low.owner == base) low.pos else if (high.owner == base) high.pos else base.pos
    def rootType: Type      = self

    def lowType: Type       = knownMemberType(knownLow, low)
    def lowErased: Type     = erasure.specialErasure(low)(low.tpe)
    def lowClassBound: Type = classBoundAsSeen(low.tpe.typeSymbol)

    def highType: Type       = knownMemberType(knownHigh, high)
    def highInfo: Type       = self memberInfo high
    def highErased: Type     = erasure.specialErasure(high)(high.tpe)
    def highClassBound: Type = classBoundAsSeen(high.tpe.typeSymbol)

    def isErroneous = low.tpe.isErroneous || high.tpe.isErroneous
    def sameKind    = sameLength(low.typeParams, high.typeParams)

    private def knownMemberType(known: MemberTypeOf, sym: Symbol): Type =
      if ((known ne null) && known.isValidFor(sym)) known.tpe else self memberType sym

    private def classBoundAsSeen(tsym: Symbol) =
      tsym.classBound.asSeenFrom(rootType, tsym.owner)

    private def memberDefString(sym: Symbol, where: Boolean) = {
      val def_s = (
        if (sym.isConstructor) s"$sym: ${self memberType sym}"
        else sym defStringSeenAs (self memberType sym)
      )
      def_s + whereString(sym)
    }
    /** A string like ' at line 55' if the symbol is defined in the class
     *  under consideration, or ' in trait Foo' if defined elsewhere.
     */
    private def whereString(sym: Symbol) =
      if (sym.owner == base) " at line " + sym.pos.line else sym.locationString

    def lowString  = memberDefString(low, where = true)
    def highString = memberDefString(high, where = true)

    override def toString = sm"""
      |Cursor(in $base) {
      |   high  $highString
      | erased  $highErased
      |  infos  ${high.infosString}
      |    low  $lowString
      | erased  $lowErased
      |  infos  ${low.infosString}
      |}""".trim
  }

  /** `self.memberType(sym)`, as computed in period `period` when `sym`'s info was `info`. */
  private final class MemberTypeOf(val sym: Symbol, val tpe: Type) {
    private[this] val period = currentPeriod
    private[this] val info = sym.rawInfo
    def isValidFor(sym: Symbol): Boolean = (sym eq this.sym) && period == currentPeriod && (sym.rawInfo eq info)
  }

  /** The cursor class
   *  @param base   the base class containing the participating symbols
   */
  abstract class Cursor(val base: Symbol) {
    cursor =>

    final val self  = base.thisType   // The type relative to which symbols are seen.

    /** A symbol for which exclude returns true will not appear as
     *  either end of a pair.
     */
    protected def exclude(sym: Symbol): Boolean

    /** Does `this.low` match `high` such that (low, high) should be
     *  considered as a pair? Types always match. Term symbols
     *  match if their member types relative to `self` match.
     */
    protected def matches(high: Symbol): Boolean

    /** Even if a pair `matches`, should the cursor skip this pair?
      *
      * @param lowClass owner of the next low symbol
      * @param highClass owner of the next hi symbol
      * @return whether to skip this pair
      */
    protected def skipOwnerPair(lowClass: Symbol, highClass: Symbol): Boolean = false

    protected def bases: List[Symbol] = base.info.baseClasses

    // All the symbols which can take part in a pair, in the order they are entered: deferred before concrete,
    // and within each, members of `Any` first and members of `base` last. The cursor walks them backwards, so a
    // member is visited before the members it shadows. `syms(nextSame(i))` is the member before `syms(i)` with the same name, or -1.
    private[this] var syms: Array[Symbol] = new Array[Symbol](64)
    private[this] var size = 0
    private[this] var nextSame: Array[Int] = null

    /** The members that have already been visited as highSymbol
     *  (but may have been excluded via hasCommonParentAsSubclass.)
     *  These will not appear as lowSymbol. A bit set indexed like `syms`.
     */
    private[this] var visited: Array[Long] = null

    /** Initialization has to run now so `syms` is populated before
     *  the declaration of curIndex.
     */
    init()

    // The current low and high symbols; the high may be null.
    private[this] var lowSymbol: Symbol  = _
    private[this] var highSymbol: Symbol = _
    def lowMemberType: Type = {
      if ((lowMemberTypeCache eq null) || (lowSymbol ne lowMemberTypeCache.sym))
        lowMemberTypeCache = new MemberTypeOf(lowSymbol, self.memberType(lowSymbol))
      lowMemberTypeCache.tpe
    }
    /** `self.memberType(high)`, remembered so that the pair formed with `high` can reuse it. */
    protected def highMemberType(high: Symbol): Type = {
      highMemberTypeCache = new MemberTypeOf(high, self.memberType(high))
      highMemberTypeCache.tpe
    }

    private[this] var lowMemberTypeCache: MemberTypeOf = _
    private[this] var highMemberTypeCache: MemberTypeOf = _

    // The current indices of the candidates for low and high symbol, or -1.
    private[this] var curIndex  = size - 1
    private[this] var nextIndex = curIndex

    // These fields are initially populated with a call to next().
    next()

    // populate the above data structures
    private def init(): Unit = {
      def add(sym: Symbol): Unit = {
        if (size == syms.length) syms = java.util.Arrays.copyOf(syms, size * 2)
        syms(size) = sym
        size += 1
      }
      // Fill `syms` with lower symbols shadowing higher ones
      def fillDecls(bcs: List[Symbol], deferred: Boolean): Unit = {
        if (!bcs.isEmpty) {
          fillDecls(bcs.tail, deferred)
          var e = bcs.head.info.decls.elems
          while (e ne null) {
            if (e.sym.initialize.isDeferred == deferred && !exclude(e.sym))
              add(e.sym)
            e = e.next
          }
        }
      }

      // first, deferred (this will need to change if we change lookup rules!)
      fillDecls(bases, deferred = true)
      // then, concrete.
      fillDecls(bases, deferred = false)

      // Link each member to the previous member with the same name, through an open-addressed table of the last index seen per name.
      val n = size
      nextSame = new Array[Int](n)
      visited = new Array[Long]((n + 63) >>> 6)
      var cap = 16
      while (cap < n * 2) cap <<= 1
      val mask = cap - 1
      val lastSeen = new Array[Int](cap) // index + 1, or 0 for an empty slot
      var i = 0
      while (i < n) {
        val name = syms(i).name
        var h = (name.hashCode * 0x9E3779B9) >>> 16 & mask
        while (lastSeen(h) != 0 && (syms(lastSeen(h) - 1).name ne name)) h = (h + 1) & mask
        nextSame(i) = lastSeen(h) - 1
        lastSeen(h) = i + 1
        i += 1
      }
    }

    private def isVisited(i: Int): Boolean = (visited(i >>> 6) & (1L << i)) != 0
    private def markVisited(i: Int): Unit = visited(i >>> 6) |= (1L << i)

    // The next member after `i` with the same name as `i`. The name is checked again, as it may have changed since `init`
    // (explicitouter expands the names of some private members, and the bridges cursor is iterated at erasure).
    private def lookupNext(i: Int): Int = {
      val name = syms(i).name
      var j = nextSame(i)
      while (j >= 0 && (syms(j).name ne name)) j = nextSame(j)
      j
    }

    // We can only draw conclusions about linearisation from a non-trait parent; skip Object, being the top of the lattice.
    protected lazy val nonTraitParent: Symbol =
      base.info.firstParent.typeSymbol.filter(sym => !sym.isTrait && sym != definitions.ObjectClass)

    // Every class has the members of `Object` that implement those of `Any` (`equals`, `hashCode`, `==`, `##`, ...),
    // and they always match. No cursor needs to see those pairs, so they are skipped without the cost of `matches`.
    private def isObjectAnyPair(low: Symbol, high: Symbol): Boolean =
      (low.owner eq definitions.ObjectClass) && (high.owner eq definitions.AnyClass)

    @tailrec private def advanceNextEntry(): Unit = {
      if (nextIndex >= 0) {
        nextIndex = lookupNext(nextIndex)
        if (nextIndex >= 0) {
          val high = syms(nextIndex)
          if (isObjectAnyPair(low, high)) {
            markVisited(nextIndex)
            advanceNextEntry()
          } else {
            val isMatch = matches(high) && { markVisited(nextIndex) ; true } // side-effect visited on all matches

            // Advance if no match, or if the particular cursor is not interested in this pair
            if (!isMatch || skipOwnerPair(low.owner, high.owner)) advanceNextEntry()
            else highSymbol = high
          }
        }
      }
    }
    @tailrec private def advanceCurEntry(): Unit = {
      if (curIndex >= 0) {
        curIndex -= 1
        if (curIndex >= 0) {
          if (isVisited(curIndex) || exclude(syms(curIndex)))
            advanceCurEntry()
          else
            nextIndex = curIndex
        }
      }
    }

    /** The `low` and `high` symbol.  In the context of overriding pairs,
     *  low == overriding and high == overridden.
     */
    def low  = lowSymbol
    def high = highSymbol

    def hasNext     = curIndex >= 0
    def currentPair = {
      def known(c: MemberTypeOf, sym: Symbol) = if ((c ne null) && (c.sym eq sym)) c else null
      new SymbolPair(base, low, high).seed(known(lowMemberTypeCache, low), known(highMemberTypeCache, high))
    }
    def iterator: Iterator[SymbolPair] = new collection.AbstractIterator[SymbolPair] {
      def hasNext = cursor.hasNext
      def next()  = try cursor.currentPair finally cursor.next()
    }

    // Note that next is called once during object initialization to
    // populate the fields tracking the current symbol pair.
    @tailrec
    final def next(): Unit = {
      if (curIndex >= 0) {
        lowSymbol = syms(curIndex)
        advanceNextEntry()        // sets highSymbol
        if (nextIndex < 0) {
          advanceCurEntry()
          next()
        }
      }
    }
  }
}
