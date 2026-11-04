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

import scala.io.Codec

trait Names extends api.Names {
  private final val HASH_SIZE  = 0x8000
  private final val HASH_MASK  = 0x7FFF
  private final val NAME_SIZE  = 0x20000

  final val nameDebug = false

  // Ideally we would just synchronize unconditionally and let HotSpot's Biased Locking
  // kick in in the compiler universe, where access to the lock is single threaded. But,
  // objects created in the first 4seconds of the JVM startup aren't eligible for biased
  // locking.
  //
  // We might also be able to accept the performance hit, but we don't have tools to
  // detect performance regressions.
  //
  // Discussion: https://groups.google.com/forum/#!search/biased$20scala-internals/scala-internals/0cYB7SkJ-nM/47MLhsgw8jwJ
  protected def synchronizeNames: Boolean = false
  private[this] val nameLock: Object = new Object


  /** Which `NameStore` holds the characters of names: `chars`, `cached`, `string` or `bytes`. See `-Yname-table`. */
  protected def nameTableKind: String = "chars"

  /** Hashtable for finding term names quickly. */
  private[this] val termHashtable = new Array[TermName](HASH_SIZE)

  /** Hashtable for finding type names quickly. */
  private[this] val typeHashtable = new Array[TypeName](HASH_SIZE)

  /** One instance per universe, and only its class is instantiated, so that a compile that uses one store pays for
   *  no others: the call sites in `Name` stay monomorphic.
   */
  private[this] val store: NameStore = nameTableKind match {
    case "chars"  => new CharStore
    case "cached" => new CachedCharStore
    case "string" => new StringStore
    case "bytes"  => new BytesStore
    case "shared" => new SharedStore(SharedNameTable.global)
    case kind     => throw new IllegalArgumentException(s"unknown name table: $kind")
  }

  @deprecated("Don't access name table contents directly.", "2.12.9")
  def chrs: Array[Char] = store.legacyChars
  @deprecated("Don't access name table contents directly.", "2.12.9")
  def chrs_=(cs: Array[Char]) = store.legacyChars = cs
  final def nameTableSize: Int = store.size

  final def allNames(): Iterator[TermName] = store.allNames

  /** The same value as `String.hashCode`, so that the stores can reuse a `String`'s cached hash. */
  private def hashValue(cs: Array[Char], offset: Int, len: Int): Int = {
    var h = 0
    var i = 0
    while (i < len) {
      h = 31 * h + cs(i + offset)
      i += 1
    }
    h
  }

  /** Create a term name from the characters in cs[offset..offset+len-1]. */
  final def newTermName(cs: Array[Char], offset: Int, len: Int): TermName =
    newTermName(cs, offset, len, cachedString = null)

  final def newTermName(cs: Array[Char]): TermName = newTermName(cs, 0, cs.length)

  final def newTypeName(cs: Array[Char]): TypeName = newTypeName(cs, 0, cs.length)

  /** Create a term name from the characters in cs[offset..offset+len-1].
   *  TODO - have a mode where name validation is performed at creation time
   *  (e.g. if a name has the string "\$class" in it, then fail if that
   *  string is not at the very end.)
   *
   *  @param len0 the length of the name. Negative lengths result in empty names.
   */
  final def newTermName(cs: Array[Char], offset: Int, len0: Int, cachedString: String): TermName = {
    def body = {
      require(offset >= 0, "offset must be non-negative, got " + offset)
      store.termFromChars(cs, offset, math.max(len0, 0), cachedString)
    }
    if (synchronizeNames) nameLock.synchronized(body) else body
  }

  /** Create a term name from a character sequence whose contents aren't otherwise needed, such as the scanner's buffer. A
   *  name that exists needs no copy of them. This has its own name so that `newTermName(aName)` stays an error.
   */
  final def newTermNameFromSequence(cs: CharSequence): TermName =
    if (synchronizeNames) nameLock.synchronized(store.termFromSequence(cs)) else store.termFromSequence(cs)

  final def newTypeName(cs: Array[Char], offset: Int, len: Int, cachedString: String): TypeName =
    newTermName(cs, offset, len, cachedString).toTypeName

  private def newTermNameFromString(s: String, cachedString: String): TermName =
    if (synchronizeNames) nameLock.synchronized(store.termFromString(s, cachedString)) else store.termFromString(s, cachedString)

  /** Create a term name from string. */
  @deprecatedOverriding("To synchronize, use `override def synchronizeNames = true`", "2.11.0") // overridden in https://github.com/scala-ide/scala-ide/blob/master/org.scala-ide.sdt.core/src/scala/tools/eclipse/ScalaPresentationCompiler.scala
  def newTermName(s: String): TermName = newTermNameFromString(s, null)

  /** Create a type name from string. */
  @deprecatedOverriding("To synchronize, use `override def synchronizeNames = true`", "2.11.0") // overridden in https://github.com/scala-ide/scala-ide/blob/master/org.scala-ide.sdt.core/src/scala/tools/eclipse/ScalaPresentationCompiler.scala
  def newTypeName(s: String): TypeName = newTermName(s).toTypeName

  /** Create a term name from the UTF8 encoded bytes in bs[offset..offset+len-1]. */
  final def newTermName(bs: Array[Byte], offset: Int, len: Int): TermName =
    if (synchronizeNames) nameLock.synchronized(store.termFromUtf8(bs, offset, len)) else store.termFromUtf8(bs, offset, len)

  final def newTermNameCached(s: String): TermName =
    newTermNameFromString(s, cachedString = s)

  final def newTypeNameCached(s: String): TypeName =
    newTermNameFromString(s, cachedString = s).toTypeName

  /** Create a type name from the characters in cs[offset..offset+len-1]. */
  final def newTypeName(cs: Array[Char], offset: Int, len: Int): TypeName =
    newTermName(cs, offset, len, cachedString = null).toTypeName

  /** Create a type name from the UTF8 encoded bytes in bs[offset..offset+len-1]. */
  final def newTypeName(bs: Array[Byte], offset: Int, len: Int): TypeName =
    newTermName(bs, offset, len).toTypeName

  /**
   * Used by the GenBCode backend to lookup type names that are known to already exist. This method
   * might be invoked in a multi-threaded setting. Invoking newTypeName instead might be unsafe.
   *
   * can-multi-thread: names are added to the hash tables only after they are fully constructed.
   */
  final def lookupTypeName(cs: Array[Char]): TypeName = {
    val hash = hashValue(cs, 0, cs.length) & HASH_MASK
    var typeName = typeHashtable(hash)

    while ((typeName ne null) && (typeName.length != cs.length || !store.matches(typeName, cs, 0, cs.length))) {
      typeName = typeName.next
    }
    assert(typeName != null, s"TypeName ${new String(cs)} not yet created.")
    typeName
  }

// Name stores ------------------------------------------------------------------

  /** Where the characters of names live, and the operations that depend on it. Positions and lengths are in UTF-16
   *  units whatever the store. The defaults go through `charAt`; the stores override what they can do better.
   *
   *  Every store hashes a name to the same value, `String.hashCode` of its contents, and keeps `Name.id` unique to a
   *  string (apart from names sharing a prefix in one range), as `Scopes`, `Trees` and the keyword tables rely on it.
   */
  private[internal] sealed abstract class NameStore {
    def size: Int
    protected def kind: String
    private def noCharTable(what: String) =
      throw new UnsupportedOperationException(s"$what is an offset into or the contents of the Array[Char] of the name table, which -Yname-table:$kind doesn't have. Use the operations of Name, such as charAt, toString and appendTo, or -Yname-table:chars.")
    def legacyChars: Array[Char] = noCharTable("Names#chrs")
    def legacyChars_=(cs: Array[Char]): Unit = noCharTable("Names#chrs")
    def legacyStart(n: Name): Int = noCharTable("Name#start")

    def termFromChars(cs: Array[Char], offset: Int, len: Int, cachedString: String): TermName
    def termFromString(s: String, cachedString: String): TermName
    def termFromUtf8(bs: Array[Byte], offset: Int, len: Int): TermName = {
      val chars = Codec.fromUTF8(bs, offset, len)
      termFromChars(chars, 0, chars.length, null)
    }
    def termFromSequence(cs: CharSequence): TermName = {
      val len = cs.length
      var h = 0
      var i = 0
      while (i < len) { h = 31 * h + cs.charAt(i); i += 1 }
      var n = termHashtable(h & HASH_MASK)
      while ((n ne null) && (n.length != len || !sameAsSequence(n, cs, len)))
        n = n.next
      if (n ne null) n
      else {
        val chars = new Array[Char](len)
        i = 0
        while (i < len) { chars(i) = cs.charAt(i); i += 1 }
        termFromChars(chars, 0, len, null)
      }
    } 
    protected def sameAsSequence(n: Name, cs: CharSequence, len: Int): Boolean = {
      var i = 0
      while (i < len && charAt(n, i) == cs.charAt(i)) i += 1
      i == len
    }
    def subName(n: Name, from: Int, to: Int): TermName
    def hash(n: Name): Int
    def matches(n: Name, cs: Array[Char], offset: Int, len: Int): Boolean
    def toString(n: Name): String
    def charAt(n: Name, i: Int): Char

    def utf8(n: Name): Array[Byte] = Codec.toUTF8(toString(n))

    def allNames: Iterator[TermName] = termHashtable.iterator.filter(_ ne null).flatMap(n => Iterator.iterate(n)(_.next).takeWhile(_ ne null))

    /** The term name of a type name, which always exists. */
    def termOf(t: TypeName): TermName = {
      // Re-computing the hash saves a field for storing it in the TypeName
      val h = hash(t) & HASH_MASK
      var n = termHashtable(h)
      while ((n ne null) && n.id != t.id)
        n = n.next
      n
    }

    def copyChars(n: Name, cs: Array[Char], offset: Int): Unit = {
      var i = 0
      while (i < n.length) { cs(offset + i) = charAt(n, i); i += 1 }
    }
    def pos(n: Name, c: Char, start: Int): Int = {
      var i = start
      while (i < n.length && charAt(n, i) != c) i += 1
      i
    }
    def pos(n: Name, s: String, start: Int): Int = {
      var i = pos(n, s.charAt(0), start)
      val sLen = s.length()
      if (sLen == 1) return i
      while (i + sLen <= n.length) {
        var j = 1
        while (s.charAt(j) == charAt(n, i + j)) {
          j += 1
          if (j == sLen) return i
        }
        i = pos(n, s.charAt(0), i + 1)
      }
      n.length
    }
    def lastPos(n: Name, c: Char, start: Int): Int = {
      var i = start
      while (i >= 0 && charAt(n, i) != c) i -= 1
      i
    }
    def startsWith(n: Name, prefix: Name, start: Int): Boolean = {
      var i = 0
      while (i < prefix.length && start + i < n.length && charAt(n, start + i) == charAt(prefix, i))
        i += 1
      i == prefix.length
    }
    def startsWith(n: Name, prefix: String, start: Int): Boolean = {
      var i = 0
      while (i < prefix.length && start + i < n.length && charAt(n, start + i) == prefix.charAt(i))
        i += 1
      i == prefix.length
    }
    def endsWith(n: Name, suffix: Name, end: Int): Boolean = {
      var i = 1
      while (i <= suffix.length && i <= end && charAt(n, end - i) == charAt(suffix, suffix.length - i))
        i += 1
      i > suffix.length
    }
    def endsWith(n: Name, suffix: String, end: Int): Boolean = {
      var i = 1
      while (i <= suffix.length && i <= end && charAt(n, end - i) == suffix.charAt(suffix.length - i))
        i += 1
      i > suffix.length
    }
    def containsName(n: Name, subname: Name): Boolean = {
      var start = 0
      val last = n.length - subname.length
      while (start <= last && !startsWith(n, subname, start)) start += 1
      start <= last
    }
    def containsChar(n: Name, ch: Char): Boolean = {
      var i = 0
      while (i < n.length) {
        if (charAt(n, i) == ch) return true
        i += 1
      }
      false
    }
    def lastIndexOf(n: Name, s: String): Int = {
      val slength   = s.length()
      val lastIndex = slength - 1
      val lastChar  = s.charAt(lastIndex)
      var end       = n.length - 1
      while (end >= lastIndex) {
        if (charAt(n, end) == lastChar) {
          var i  = end - 1
          val i0 = i - lastIndex
          var at = lastIndex - 1
          while (i > i0 && charAt(n, i) == s.charAt(at)) {
            i -= 1
            at -= 1
          }
          if (i == i0) return i0 + 1
        }
        end -= 1
      }
      -1
    }
    def appendTo(n: Name, buffer: java.lang.StringBuffer, start: Int, length: Int): Unit =
      buffer.append(n: CharSequence, start, start + length)
    def appendTo(n: Name, sb: StringBuilder, start: Int, length: Int): Unit =
      sb.underlying.append(n: CharSequence, start, start + length)

    /** Puts a new term name at the head of bucket `h`, adding it only after it's been fully constructed. */
    protected final def addTerm(h: Int, start: Int, len: Int, str: String): TermName = {
      val termName = new TermName(start, len, termHashtable(h), str)
      termHashtable(h) = termName
      termName
    }
    }

  /** All names sequentially in one `Array[Char]`, a name being an offset and a length. */
  private[internal] sealed abstract class CharBackedStore extends NameStore {
    protected[this] var _chrs: Array[Char] = new Array[Char](NAME_SIZE)
    protected[this] var nc = 0
    final def size: Int = nc
    final override def legacyChars: Array[Char] = _chrs
    final override def legacyChars_=(cs: Array[Char]): Unit = _chrs = cs
    final override def legacyStart(n: Name): Int = n.id

    /** Is (the ASCII representation of) name at given index equal to
     *  cs[offset..offset+len-1]?
     */
    private def equals(index: Int, cs: Array[Char], offset: Int, len: Int): Boolean = {
      var i = 0
      val chrs = _chrs
      while ((i < len) && (chrs(index + i) == cs(offset + i)))
        i += 1
      i == len
    }

    /** Enter characters into chrs array. */
    private def enterChars(cs: Array[Char], offset: Int, len: Int): Unit = {
      var i = 0
      while (i < len) {
        if (nc + i == _chrs.length) {
          val newchrs = new Array[Char](_chrs.length * 2)
          java.lang.System.arraycopy(_chrs, 0, newchrs, 0, _chrs.length)
          _chrs = newchrs
        }
        _chrs(nc + i) = cs(offset + i)
        i += 1
      }
      if (len == 0) nc += 1
      else nc = nc + len
    }

    final def termFromChars(cs: Array[Char], offset: Int, len: Int, cachedString: String): TermName = {
      val h = hashValue(cs, offset, len) & HASH_MASK
      var n = termHashtable(h)
      while ((n ne null) && (n.length != len || !equals(n.id, cs, offset, len)))
        n = n.next

      if (n ne null) n
      else {
        // The logic order here is future-proofing against the possibility
        // that name.toString will become an eager val, in which case the call
        // to enterChars cannot follow the construction of the TermName.
        var startIndex = 0
        if (cs == _chrs) {
          // Optimize for subName, the new name is already stored in chrs
          startIndex = offset
        } else {
          startIndex = nc
          enterChars(cs, offset, len)
        }
        addTerm(h, startIndex, len, cachedString)
      }
    }
    final def subName(n: Name, from: Int, to: Int): TermName = {
      val offset = n.id + from
      require(offset >= 0, "offset must be non-negative, got " + offset)
      termFromChars(_chrs, offset, math.max(to - from, 0), null)
    }
    def hash(n: Name): Int = hashValue(_chrs, n.id, n.length)
    final def matches(n: Name, cs: Array[Char], offset: Int, len: Int): Boolean = equals(n.id, cs, offset, len)
    final def charAt(n: Name, i: Int): Char = _chrs(n.id + i)

    final override def copyChars(n: Name, cs: Array[Char], offset: Int): Unit =
      java.lang.System.arraycopy(_chrs, n.id, cs, offset, n.length)
    final override def pos(n: Name, c: Char, start: Int): Int = {
      var i = start
      val index = n.id
      val len = n.length
      val chrs = _chrs
      while (i < len && chrs(index + i) != c) i += 1
      i
    }
    final override def pos(n: Name, s: String, start: Int): Int = {
      var i = pos(n, s.charAt(0), start)
      val sLen = s.length()
      if (sLen == 1) return i
      val index = n.id
      val len = n.length
      val chrs = _chrs
      while (i + sLen <= len) {
        var j = 1
        while (s.charAt(j) == chrs(index + i + j)) {
          j += 1
          if (j == sLen) return i
        }
        i = pos(n, s.charAt(0), i + 1)
      }
      len
    }
    final override def lastPos(n: Name, c: Char, start: Int): Int = {
      var i = start
      val index = n.id
      val chrs = _chrs
      while (i >= 0 && chrs(index + i) != c) i -= 1
      i
    }
    final override def startsWith(n: Name, prefix: Name, start: Int): Boolean = {
      val chrs = _chrs
      val index = n.id
      val len = n.length
      var i = 0
      while (i < prefix.length && start + i < len &&
             chrs(index + start + i) == chrs(prefix.id + i))
        i += 1
      i == prefix.length
    }
    final override def startsWith(n: Name, prefix: String, start: Int): Boolean = {
      val chrs = _chrs
      val index = n.id
      val len = n.length
      var i = 0
      while (i < prefix.length && start + i < len &&
             chrs(index + start + i) == prefix.charAt(i))
        i += 1
      i == prefix.length
    }
    final override def endsWith(n: Name, suffix: Name, end: Int): Boolean = {
      var i = 1
      val chrs = _chrs
      val index = n.id
      while (i <= suffix.length && i <= end &&
             chrs(index + end - i) == chrs(suffix.id + suffix.length - i))
        i += 1
      i > suffix.length
    }
    final override def endsWith(n: Name, suffix: String, end: Int): Boolean = {
      var i = 1
      val chrs = _chrs
      val index = n.id
      while (i <= suffix.length && i <= end &&
             chrs(index + end - i) == suffix.charAt(suffix.length - i))
        i += 1
      i > suffix.length
    }
    final override def containsChar(n: Name, ch: Char): Boolean = {
      var i = n.id
      val max = i + n.length
      val chrs = _chrs
      while (i < max) {
        if (chrs(i) == ch)
          return true
        i += 1
      }
      false
    }
    final override def lastIndexOf(n: Name, s: String): Int = {
      val slength   = s.length()
      val lastIndex = slength - 1
      val lastChar  = s.charAt(lastIndex)
      val contents  = _chrs
      val base      = n.id
      val min       = base + lastIndex
      var end       = base + n.length - 1

      while (end >= min) {
        if (contents(end) == lastChar) {
          var i  = end - 1
          val i0 = i - lastIndex
          var at = lastIndex - 1
          while (i > i0 && contents(i) == s.charAt(at)) {
            i -= 1
            at -= 1
          }
          if (i == i0) return i0 + 1 - base
        }
        end -= 1
      }
      -1
    }
    final override def appendTo(n: Name, buffer: java.lang.StringBuffer, start: Int, length: Int): Unit =
      buffer.append(_chrs, n.id + start, length)
    final override def appendTo(n: Name, sb: StringBuilder, start: Int, length: Int): Unit =
      sb.appendAll(_chrs, n.id + start, length)
  }

  /** The table as it has been. */
  private[internal] final class CharStore extends CharBackedStore {
    protected def kind = "chars"
    def termFromString(s: String, cachedString: String): TermName = termFromChars(s.toCharArray(), 0, s.length(), cachedString)
    def toString(n: Name): String = if (n.str == null) new String(_chrs, n.id, n.length) else n.str
  }

  /** The same table, but `toString` keeps its result, as nearly all of its calls repeat an earlier one, and a `String`
   *  enters a name without a temporary array.
   */
  private[internal] final class CachedCharStore extends CharBackedStore {
    protected def kind = "cached"
    def termFromString(s: String, cachedString: String): TermName = {
      val len = s.length()
      val h = s.hashCode & HASH_MASK
      var n = termHashtable(h)
      while ((n ne null) && (n.length != len || !sameAs(n.id, s, len)))
        n = n.next
      if (n ne null) n
      else {
        val startIndex = nc
        var capacity = _chrs.length
        while (nc + len > capacity) capacity *= 2
        if (capacity != _chrs.length) {
          val newchrs = new Array[Char](capacity)
          java.lang.System.arraycopy(_chrs, 0, newchrs, 0, nc)
          _chrs = newchrs
        }
        s.getChars(0, len, _chrs, nc)
        if (len == 0) nc += 1
        else nc = nc + len
        addTerm(h, startIndex, len, cachedString)
      }
    }
    private def sameAs(index: Int, s: String, len: Int): Boolean = {
      val chrs = _chrs
      var i = 0
      while (i < len && chrs(index + i) == s.charAt(i)) i += 1
      i == len
    }
    def toString(n: Name): String = {
      var s = n.str
      if (s == null) {
        s = new String(_chrs, n.id, n.length)
        n.str = s // a data race at worst builds it twice
      }
      s
    }
    override def hash(n: Name): Int = { val s = n.str; if (s == null) super.hash(n) else s.hashCode }
  }

  /** The operations of a store whose names each hold a `String`, with `Name.id` a serial number. */
  private[internal] sealed abstract class StringBackedStore extends NameStore {
    final def hash(n: Name): Int = n.str.hashCode
    final def matches(n: Name, cs: Array[Char], offset: Int, len: Int): Boolean = {
      val s = n.str
      var i = 0
      while (i < len && s.charAt(i) == cs(offset + i)) i += 1
      i == len
    }
    final def toString(n: Name): String = n.str
    final def charAt(n: Name, i: Int): Char = n.str.charAt(i)

    final override def copyChars(n: Name, cs: Array[Char], offset: Int): Unit = n.str.getChars(0, n.length, cs, offset)
    final override def pos(n: Name, c: Char, start: Int): Int = { val i = n.str.indexOf(c.toInt, start); if (i < 0) math.max(n.length, start) else i }
    final override def pos(n: Name, s: String, start: Int): Int = { val i = n.str.indexOf(s, start); if (i < 0) n.length else i }
    final override def lastPos(n: Name, c: Char, start: Int): Int = n.str.lastIndexOf(c.toInt, start)
    final override def startsWith(n: Name, prefix: Name, start: Int): Boolean = n.str.startsWith(prefix.str, start)
    final override def startsWith(n: Name, prefix: String, start: Int): Boolean = n.str.startsWith(prefix, start)
    final override def endsWith(n: Name, suffix: Name, end: Int): Boolean = end >= suffix.length && n.str.startsWith(suffix.str, end - suffix.length)
    final override def endsWith(n: Name, suffix: String, end: Int): Boolean = end >= suffix.length && n.str.startsWith(suffix, end - suffix.length)
    final override def containsName(n: Name, subname: Name): Boolean = n.str.contains(subname.str)
    final override def containsChar(n: Name, ch: Char): Boolean = n.str.indexOf(ch.toInt) >= 0
    final override def lastIndexOf(n: Name, s: String): Int = n.str.lastIndexOf(s)
    final override def appendTo(n: Name, buffer: java.lang.StringBuffer, start: Int, length: Int): Unit =
      buffer.append(n.str, start, start + length)
    final override def appendTo(n: Name, sb: StringBuilder, start: Int, length: Int): Unit =
      sb.underlying.append(n.str, start, start + length)
  }

  /** Each name holds a `String`. This is how Scala 3's `SimpleName` works. */
  private[internal] final class StringStore extends StringBackedStore {
    protected def kind = "string"
    private[this] var nextId = 0
    def size: Int = nextId

    private def add(h: Int, s: String): TermName = {
      val id = nextId
      nextId += 1
      addTerm(h, id, s.length, s)
    }
    def termFromChars(cs: Array[Char], offset: Int, len: Int, cachedString: String): TermName = {
      val h = hashValue(cs, offset, len) & HASH_MASK
      var n = termHashtable(h)
      while ((n ne null) && (n.length != len || !matches(n, cs, offset, len)))
        n = n.next
      if (n ne null) n
      else add(h, if (cachedString ne null) cachedString else new String(cs, offset, len))
    }
    def termFromString(s: String, cachedString: String): TermName = {
      val h = s.hashCode & HASH_MASK
      var n = termHashtable(h)
      while ((n ne null) && !n.str.equals(s))
        n = n.next
      if (n ne null) n else add(h, s)
    }
    override def termFromUtf8(bs: Array[Byte], offset: Int, len: Int): TermName = {
      var h = 0
      var or = 0
      var i = 0
      while (i < len) {
        val b = bs(offset + i)
        or |= b
        h = 31 * h + b
        i += 1
      }
      if (or < 0) termFromString(new String(bs, offset, len, java.nio.charset.StandardCharsets.UTF_8), null)
      else {
        h &= HASH_MASK
        var n = termHashtable(h)
        while ((n ne null) && (n.length != len || !sameBytes(n.str, bs, offset, len)))
          n = n.next
        if (n ne null) n
        else add(h, new String(bs, offset, len, java.nio.charset.StandardCharsets.ISO_8859_1))
      }
    }
    private def sameBytes(s: String, bs: Array[Byte], offset: Int, len: Int): Boolean = {
      var i = 0
      while (i < len && s.charAt(i) == bs(offset + i)) i += 1
      i == len
    }
    def subName(parent: Name, from: Int, to: Int): TermName = {
      val s = parent.str
      val len = math.max(to - from, 0)
      var h = 0
      var i = 0
      while (i < len) { h = 31 * h + s.charAt(from + i); i += 1 }
      h &= HASH_MASK
      var n = termHashtable(h)
      while ((n ne null) && (n.length != len || !n.str.regionMatches(0, s, from, len)))
        n = n.next
      if (n ne null) n else add(h, s.substring(from, from + len))
    }
  }

  /** A name holds a `String` from a table that many universes can share, and is found through a `ConcurrentHashMap`,
   *  so that entering a name needs no lock here or in the shared table.
   *
   *  `Name`s themselves stay per universe: each has the universe's `$outer`, which the outer tests of patterns like
   *  `_: TermName` check. What the universes share is the strings and `Name.id`.
   */
  private[internal] final class SharedStore(shared: SharedNameTable) extends StringBackedStore {
    protected def kind = "shared"
    private[this] val terms = new java.util.concurrent.ConcurrentHashMap[String, TermName](0x8000)
    def size: Int = terms.size

    override def allNames: Iterator[TermName] = {
      val it = terms.values.iterator
      new Iterator[TermName] { def hasNext = it.hasNext; def next() = it.next() }
    }
    override def termOf(t: TypeName): TermName = terms.get(t.str)

    private[this] val probes = new ThreadLocal[SharedNameTable.Probe] {
      override def initialValue = new SharedNameTable.Probe
    }

    private def add(s: String): TermName = {
      var entry = shared.intern(s)
      var str = entry.get
      while (str eq null) { // the entry's string has gone since it was found
        entry = shared.intern(s)
        str = entry.get
      }
      val term = new TermName(entry.id, s.length, null, str)
      val raced = terms.putIfAbsent(str, term)
      if (raced eq null) term else raced
    }

    /** The name the probe describes, building a `String` only if neither table has one. */
    private def lookup(probe: SharedNameTable.Probe, cachedString: String): TermName = {
      val found = terms.get(probe)
      val result =
        if (found ne null) found
        else {
          val known = shared.find(probe)
          add(if (known ne null) known else if (cachedString ne null) cachedString else probe.newString())
        }
      probe.clear()
      result
    }
    def termFromChars(cs: Array[Char], offset: Int, len: Int, cachedString: String): TermName = {
      val probe = probes.get
      probe.setChars(cs, offset, len, hashValue(cs, offset, len))
      lookup(probe, cachedString)
    }
    override def termFromSequence(cs: CharSequence): TermName = {
      val len = cs.length
      var h = 0
      var i = 0
      while (i < len) { h = 31 * h + cs.charAt(i); i += 1 }
      val probe = probes.get
      probe.setSequence(cs, len, h)
      lookup(probe, null)
    }
    def termFromString(s: String, cachedString: String): TermName = {
      val found = terms.get(s)
      if (found ne null) found else add(s)
    }
    override def termFromUtf8(bs: Array[Byte], offset: Int, len: Int): TermName = {
      var h = 0
      var or = 0
      var i = 0
      while (i < len) {
        val b = bs(offset + i)
        or |= b
        h = 31 * h + b
        i += 1
      }
      if (or < 0) termFromString(new String(bs, offset, len, java.nio.charset.StandardCharsets.UTF_8), null)
      else {
        val probe = probes.get
        probe.setAscii(bs, offset, len, h)
        lookup(probe, null)
      }
    }
    def subName(parent: Name, from: Int, to: Int): TermName = {
      val s = parent.str
      val len = math.max(to - from, 0)
      var h = 0
      var i = 0
      while (i < len) { h = 31 * h + s.charAt(from + i); i += 1 }
      val probe = probes.get
      probe.setRegion(s, from, len, h)
      lookup(probe, null)
    }
  }

  /** Two tables, as compact strings has two codings: names that are all ASCII are bytes in one, the others are chars
   *  in the other. A name's contents pick the table, so a string is only ever in one of them. `Name.id` is the
   *  offset in the bytes, or the complement of the offset in the chars.
   */
  private[internal] final class BytesStore extends NameStore {
    protected def kind = "bytes"
    private[this] var bytes = new Array[Byte](NAME_SIZE)
    private[this] var nb = 0
    private[this] var chars = new Array[Char](0)
    private[this] var nc = 0
    def size: Int = nb + nc

    private def reserveBytes(len: Int): Unit = if (nb + len > bytes.length) {
      var capacity = bytes.length
      while (nb + len > capacity) capacity *= 2
      val grown = new Array[Byte](capacity)
      java.lang.System.arraycopy(bytes, 0, grown, 0, nb)
      bytes = grown
    }
    private def reserveChars(len: Int): Unit = if (nc + len > chars.length) {
      var capacity = math.max(chars.length, 256)
      while (nc + len > capacity) capacity *= 2
      val grown = new Array[Char](capacity)
      java.lang.System.arraycopy(chars, 0, grown, 0, nc)
      chars = grown
    }
    private def advanceBytes(len: Int): Unit = nb += (if (len == 0) 1 else len)

    def termFromChars(cs: Array[Char], offset: Int, len: Int, cachedString: String): TermName = {
      var h = 0
      var or = 0
      var i = 0
      while (i < len) {
        val c = cs(offset + i)
        or |= c
        h = 31 * h + c
        i += 1
      }
      h &= HASH_MASK
      val ascii = or < 0x80
      var n = termHashtable(h)
      while ((n ne null) && (n.length != len || ascii != (n.id >= 0) || !matches(n, cs, offset, len)))
        n = n.next
      if (n ne null) n
      else if (ascii) {
        val start = nb
        reserveBytes(len)
        val to = bytes
        i = 0
        while (i < len) { to(start + i) = cs(offset + i).toByte; i += 1 }
        advanceBytes(len)
        addTerm(h, start, len, cachedString)
      } else if (cs eq chars) addTerm(h, ~offset, len, cachedString) // a subName, already in the chars
      else {
        val start = nc
        reserveChars(len)
        java.lang.System.arraycopy(cs, offset, chars, start, len)
        nc += len
        addTerm(h, ~start, len, cachedString)
      }
    }
    def termFromString(s: String, cachedString: String): TermName = {
      val len = s.length()
      var or = 0
      var i = 0
      while (i < len) { or |= s.charAt(i); i += 1 }
      val h = s.hashCode & HASH_MASK
      val ascii = or < 0x80
      var n = termHashtable(h)
      while ((n ne null) && (n.length != len || ascii != (n.id >= 0) || !sameAs(n, s, len)))
        n = n.next
      if (n ne null) n
      else if (ascii) {
        val start = nb
        reserveBytes(len)
        val to = bytes
        i = 0
        while (i < len) { to(start + i) = s.charAt(i).toByte; i += 1 }
        advanceBytes(len)
        addTerm(h, start, len, cachedString)
      } else {
        val start = nc
        reserveChars(len)
        s.getChars(0, len, chars, start)
        nc += len
        addTerm(h, ~start, len, cachedString)
      }
    }
    override def termFromUtf8(bs: Array[Byte], offset: Int, len: Int): TermName = {
      var h = 0
      var or = 0
      var i = 0
      while (i < len) {
        val b = bs(offset + i)
        or |= b
        h = 31 * h + b
        i += 1
      }
      if (or < 0) super.termFromUtf8(bs, offset, len)
      else termFromAsciiBytes(bs, offset, len, h & HASH_MASK)
    }
    private def termFromAsciiBytes(bs: Array[Byte], offset: Int, len: Int, h: Int): TermName = {
      var n = termHashtable(h)
      while ((n ne null) && (n.length != len || n.id < 0 || !sameBytes(n.id, bs, offset, len)))
        n = n.next
      if (n ne null) n
      else if (bs eq bytes) addTerm(h, offset, len, null) // a subName, already in the bytes
      else {
        val start = nb
        reserveBytes(len)
        java.lang.System.arraycopy(bs, offset, bytes, start, len)
        advanceBytes(len)
        addTerm(h, start, len, null)
      }
    }
    private def sameBytes(index: Int, bs: Array[Byte], offset: Int, len: Int): Boolean = {
      val from = bytes
      var i = 0
      while (i < len && from(index + i) == bs(offset + i)) i += 1
      i == len
    }
    private def sameAs(n: Name, s: String, len: Int): Boolean = {
      var i = 0
      while (i < len && charAt(n, i) == s.charAt(i)) i += 1
      i == len
    }
    def subName(parent: Name, from: Int, to: Int): TermName = {
      val len = math.max(to - from, 0)
      val start = parent.id
      if (start < 0) termFromChars(chars, ~start + from, len, null) // may be all ASCII, and then goes to the bytes
      else {
        val from0 = start + from
        val bs = bytes
        var h = 0
        var i = 0
        while (i < len) { h = 31 * h + bs(from0 + i); i += 1 }
        termFromAsciiBytes(bs, from0, len, h & HASH_MASK)
      }
    }
    def hash(n: Name): Int = {
      val start = n.id
      if (start < 0) hashValue(chars, ~start, n.length)
      else {
        val bs = bytes
        val len = n.length
        var h = 0
        var i = 0
        while (i < len) { h = 31 * h + bs(start + i); i += 1 }
        h
      }
    }
    def matches(n: Name, cs: Array[Char], offset: Int, len: Int): Boolean = {
      var i = 0
      while (i < len && charAt(n, i) == cs(offset + i)) i += 1
      i == len
    }
    def toString(n: Name): String =
      if (n.str != null) n.str
      else {
        val start = n.id
        if (start < 0) new String(chars, ~start, n.length)
        else new String(bytes, start, n.length, java.nio.charset.StandardCharsets.ISO_8859_1)
      }
    def charAt(n: Name, i: Int): Char = {
      val start = n.id
      if (start < 0) chars(~start + i) else bytes(start + i).toChar
    }
    override def utf8(n: Name): Array[Byte] = {
      val start = n.id
      if (start < 0) super.utf8(n)
      else java.util.Arrays.copyOfRange(bytes, start, start + n.length)
    }
    override def pos(n: Name, c: Char, start: Int): Int = {
      val index = n.id
      if (index < 0) super.pos(n, c, start)
      else {
        val len = n.length
        if (c >= 0x80) math.max(len, start)
        else {
          val bs = bytes
          val b = c.toByte
          var i = start
          while (i < len && bs(index + i) != b) i += 1
          i
        }
      }
    }
    override def startsWith(n: Name, prefix: Name, start: Int): Boolean = {
      val index = n.id
      val pindex = prefix.id
      if ((index | pindex) < 0) super.startsWith(n, prefix, start)
      else {
        val bs = bytes
        val plen = prefix.length
        val len = n.length
        var i = 0
        while (i < plen && start + i < len && bs(index + start + i) == bs(pindex + i)) i += 1
        i == plen
      }
    }
    override def startsWith(n: Name, prefix: String, start: Int): Boolean = {
      val index = n.id
      if (index < 0) super.startsWith(n, prefix, start)
      else {
        val bs = bytes
        val plen = prefix.length
        val len = n.length
        var i = 0
        while (i < plen && start + i < len && bs(index + start + i) == prefix.charAt(i)) i += 1
        i == plen
      }
    }
    override def endsWith(n: Name, suffix: String, end: Int): Boolean = {
      val index = n.id
      if (index < 0) super.endsWith(n, suffix, end)
      else {
        val bs = bytes
        val slen = suffix.length
        var i = 1
        while (i <= slen && i <= end && bs(index + end - i) == suffix.charAt(slen - i)) i += 1
        i > slen
      }
    }
  }

// Classes ----------------------------------------------------------------------

  // Dummy trait to make Name#isEmpty with override keyword at JDK before 15
  sealed trait NameHasIsEmpty {
    def isEmpty: Boolean
  }

  /** The name class.
   *  TODO - resolve schizophrenia regarding whether to treat Names as Strings
   *  or Strings as Names.  Give names the key functions the absence of which
   *  make people want Strings all the time.
   */
  sealed abstract class Name(protected val index: Int, protected val len: Int, private[Names] var str: String) extends NameApi with NameHasIsEmpty with CharSequence {
    type ThisNameType >: Null <: Name
    protected[this] def thisName: ThisNameType

    /** Unique to this name's string, apart from names sharing a prefix in one range of a table, which then differ in
     *  length. A term name and its type name have the same one. This is for hashing and identity, in any name table.
     */
    private[scala] final def id: Int = index

    /** Index into name table, in `chrs`. Only some name tables have one. */
    final def start: Int = store.legacyStart(this)

    /** The next name in the same hash bucket. */
    def next: ThisNameType

    /** The length of this name. */
    final def length: Int = len
    final def nonEmpty = !isEmpty
    // This method implements NameHasIsEmpty, and overrides CharSequence's isEmpty on JDK 15+
    override final def isEmpty = length == 0

    def nameKind: String
    def isTermName: Boolean
    def isTypeName: Boolean
    def toTermName: TermName
    def toTypeName: TypeName
    def companionName: Name

    final def asTypeOf[N <: Name](other: N): N =
      (if (other.isTermName) toTermName else toTypeName).asInstanceOf[N]

    /** Return the subname with characters from from to to-1. */
    def subName(from: Int, to: Int): ThisNameType

    override def subSequence(from: Int, to: Int): CharSequence = subName(from, to)

    /** Return a new name of the same variety. */
    def newName(str: String): ThisNameType

    /** Return a new name based on string transformation. */
    def mapName(f: String => String): ThisNameType = newName(f(toString))

    /** Copy bytes of this name to buffer cs, starting at position `offset`. */
    final def copyChars(cs: Array[Char], offset: Int): Unit =
      store.copyChars(this, cs, offset)

    /** @return the ascii representation of this name */
    final def toChars: Array[Char] = {  // used by ide
      val cs = new Array[Char](len)
      copyChars(cs, 0)
      cs
    }

    /** @return the hash value of this name */
    final override def hashCode(): Int = index

    /** @return true if the string value of this name is equal
     *  to the string value of the given name or String.
     */
    def string_==(that: Name): Boolean   = (that ne null) && (toString == that.toString)
    def string_==(that: String): Boolean = (that ne null) && (toString == that)

    /****
     *  This has been quite useful to find places where people are comparing
     *  a TermName and a TypeName, or a Name and a String.

    override def equals(other: Any) = paranoidEquals(other)
    private def paranoidEquals(other: Any): Boolean = {
      val cmp = this eq other.asInstanceOf[AnyRef]
      if (cmp || !nameDebug)
        return cmp

      other match {
        case x: String  =>
          Console.println(s"Compared $debugString and String '$x'")
        case x: Name    =>
          if (this.isTermName != x.isTermName) {
            val panic = this.toTermName == x.toTermName
            Console.println("Compared '%s' and '%s', one term, one type.%s".format(this, x,
              if (panic) "  And they contain the same name string!"
              else ""
            ))
          }
        case _ =>
      }
      false
    }
    ****/

    /** @return the i'th Char of this name */
    final def charAt(i: Int): Char = store.charAt(this, i)

    /** @return the index of first occurrence of char c in this name, length if not found */
    final def pos(c: Char): Int = pos(c, 0)

    /** @return the index of first occurrence of s in this name, length if not found */
    final def pos(s: String): Int = pos(s, 0)

    /** Returns the index of the first occurrence of character c in
     *  this name from start, length if not found.
     *
     *  @param c     the character
     *  @param start the index from which to search
     *  @return      the index of the first occurrence of c
     */
    final def pos(c: Char, start: Int): Int = store.pos(this, c, start)

    /** Returns the index of the first occurrence of nonempty string s
     *  in this name from start, length if not found.
     *
     *  @param s     the string
     *  @param start the index from which to search
     *  @return      the index of the first occurrence of s
     */
    final def pos(s: String, start: Int): Int = store.pos(this, s, start)

    /** Returns the index of last occurrence of char c in this
     *  name, -1 if not found.
     *
     *  @param c the character
     *  @return  the index of the last occurrence of c
     */
    final def lastPos(c: Char): Int = lastPos(c, len - 1)

    /** Returns the index of the last occurrence of char c in this
     *  name from start, -1 if not found.
     *
     *  @param c     the character
     *  @param start the index from which to search
     *  @return      the index of the last occurrence of c
     */
    final def lastPos(c: Char, start: Int): Int = store.lastPos(this, c, start)

    /** Does this name start with prefix? */
    final def startsWith(prefix: Name): Boolean = startsWith(prefix, 0)

    /** Does this name start with prefix at given start index? */
    final def startsWith(prefix: Name, start: Int): Boolean = store.startsWith(this, prefix, start)
    final def startsWith(prefix: String, start: Int): Boolean = store.startsWith(this, prefix, start)

    /** Does this name end with suffix? */
    final def endsWith(suffix: Name): Boolean = endsWith(suffix, len)

    /** Does this name end with suffix just before given end index? */
    final def endsWith(suffix: Name, end: Int): Boolean = store.endsWith(this, suffix, end)
    final def endsWith(suffix: String, end: Int): Boolean = store.endsWith(this, suffix, end)

    final def containsName(subname: String): Boolean = containsName(newTermName(subname))
    final def containsName(subname: Name): Boolean = store.containsName(this, subname)
    final def containsChar(ch: Char): Boolean = store.containsChar(this, ch)

    def lastIndexOf(s: String): Int = if (s.isEmpty) length else store.lastIndexOf(this, s)

    /** Some thoroughly self-explanatory convenience functions.  They
     *  assume that what they're being asked to do is known to be valid.
     */
    final def startChar: Char                   = charAt(0)
    final def endChar: Char                     = charAt(len - 1)
    final def startsWith(char: Char): Boolean   = len > 0 && startChar == char
    final def startsWith(name: String): Boolean = startsWith(name, 0)
    final def endsWith(char: Char): Boolean     = len > 0 && endChar == char
    final def endsWith(name: String): Boolean   = endsWith(name, len)

    /** Rewrite the confusing failure indication via result == length to
     *  the normal failure indication via result == -1.
     */
    private def fixIndexOf(idx: Int): Int = if (idx == length) -1 else idx

    def indexOf(ch: Char)                 = fixIndexOf(pos(ch))
    def indexOf(ch: Char, fromIndex: Int) = fixIndexOf(pos(ch, fromIndex))
    def indexOf(s: String)                = fixIndexOf(pos(s))

    /** The lastPos methods already return -1 on failure. */
    def lastIndexOf(ch: Char): Int  = lastPos(ch)

    /** Replace all occurrences of `from` by `to` in
     *  name; result is always a term name.
     */
    def replace(from: Char, to: Char): Name = {
      val cs = new Array[Char](len)
      var i = 0
      while (i < len) {
        val ch = charAt(i)
        cs(i) = if (ch == from) to else ch
        i += 1
      }
      newTermName(cs, 0, len)
    }

    /* TODO - reconcile/fix that encode returns a Name but
     *  decode returns a String.
     */

    /** !!! Duplicative but consistently named.
     */
    def decoded: String = decode
    def encoded: String = "" + encode
    // def decodedName: ThisNameType = newName(decoded)
    def encodedName: ThisNameType = encode

    /** Replace operator symbols by corresponding \$op_name. */
    def encode: ThisNameType = {
      val res = NameTransformer.encodeCharSequence(this)
      if (res eq this) thisName else newName(res.toString)
    }

    /** Replace \$op_name by corresponding operator symbol. */
    def decode: String = {
      if (this containsChar '$') {
        val str = toString
        val res = NameTransformer.decode(str)
        if (res == str) str
        else res
      }
      else toString
    }

    /** TODO - find some efficiency. */
    def append(ch: Char)        = newName(toString + ch)
    def append(suffix: String)  = newName(toString + suffix)
    def append(suffix: Name)    = newName(toString + suffix)
    def append(separator: Char, suffix: Name) = newName(toString + separator + suffix)
    def prepend(prefix: String) = newName("" + prefix + this)

    def stripSuffix(suffix: String): ThisNameType =
      if (endsWith(suffix)) dropRight(suffix.length) else thisName // OPT avoid creating a Name with `suffix`

    def stripSuffix(suffix: Name): ThisNameType =
      if (endsWith(suffix)) dropRight(suffix.length) else thisName

    def take(n: Int): ThisNameType      = subName(0, n)
    def drop(n: Int): ThisNameType      = subName(n, length)
    def dropRight(n: Int): ThisNameType = subName(0, length - n)

    def dropLocal: TermName      = toTermName stripSuffix NameTransformer.LOCAL_SUFFIX_STRING
    def dropSetter: TermName     = toTermName stripSuffix NameTransformer.SETTER_SUFFIX_STRING
    def dropModule: ThisNameType = stripSuffix(NameTransformer.MODULE_SUFFIX_STRING)
    def localName: TermName      = getterName append NameTransformer.LOCAL_SUFFIX_STRING
    def setterName: TermName     = getterName append NameTransformer.SETTER_SUFFIX_STRING
    def getterName: TermName     = dropTraitSetterSeparator.dropSetter.dropLocal
    def extensionName: TermName  = append("$extension").toTermName

    private def dropTraitSetterSeparator: TermName =
      indexOf(NameTransformer.TRAIT_SETTER_SEPARATOR_STRING) match {
        case -1  => toTermName
        case idx => toTermName.drop(idx + NameTransformer.TRAIT_SETTER_SEPARATOR_STRING.length)
      }

    def decodedName: ThisNameType = newName(decode)
    def isOperatorName: Boolean   = decoded != toString
    def longString: String        = s"$nameKind $decoded"
    def debugString               = if (isTypeName) s"$decoded!" else decoded

    final def toStringWithSuffix(suffix: String): String = {
      val builder = new java.lang.StringBuilder(length + suffix.length)
      builder.append(this: CharSequence)
      builder.append(suffix)
      builder.toString
    }

    override final def toString: String = store.toString(this)
    final def appendTo(buffer: java.lang.StringBuffer, start: Int, length: Int): Unit =
      store.appendTo(this, buffer, start, length)
    final def appendTo(sb: StringBuilder, start: Int, length: Int): sb.type = {
      store.appendTo(this, sb, start, length)
      sb
    }

    /** The UTF-8 encoding of this name, a new array. */
    private[scala] final def toUTF8: Array[Byte] = store.utf8(this)
  }

  implicit val NameTag: ClassTag[Name] = ClassTag[Name](classOf[Name])

  /** A name that contains no operator chars nor dollar signs.
   *  TODO - see if it's any faster to do something along these lines.
   *  Cute: now that exhaustivity kind of works, the mere presence of
   *  this trait causes TermName and TypeName to stop being exhaustive.
   *  Commented out.
   */
  // trait AlphaNumName extends Name {
  //   final override def encode         = thisName
  //   final override def decodedName    = thisName
  //   final override def decode         = toString
  //   final override def isOperatorName = false
  // }


  // SYNCNOTE: caller to constructor must synchronize if `synchronizeNames` is enabled
  final class TermName(index0: Int, len0: Int, val next: TermName, str0: String) extends Name(index0, len0, str0) with TermNameApi {
    type ThisNameType = TermName
    protected[this] def thisName: TermName = this

    def isTermName: Boolean = true
    def isTypeName: Boolean = false
    def toTermName: TermName = this
    def toTypeName: TypeName = {
      def body = {
        // Re-computing the hash saves a field for storing it in the TermName
        val h = store.hash(this) & HASH_MASK
        var n = typeHashtable(h)
        while ((n ne null) && n.id != index)
          n = n.next

        if (n ne null) n
        else {
          val next = typeHashtable(h)
          val typeName = createCompanionName(next)
          // Add the new typeName to the hashtable only after it's been fully constructed
          typeHashtable(h) = typeName
          typeName
        }
      }
      if (synchronizeNames) nameLock.synchronized(body) else body
    }
    def newName(str: String): TermName = newTermName(str)
    def companionName: TypeName = toTypeName
    def subName(from: Int, to: Int): TermName =
      if (synchronizeNames) nameLock.synchronized(store.subName(this, from, to)) else store.subName(this, from, to)

    def nameKind = "term"
    private def createCompanionName(next: TypeName): TypeName = new TypeName(index, len, next, str)
  }

  implicit val TermNameTag: ClassTag[TermName] = ClassTag[TermName](classOf[TermName])

  object TermName extends TermNameExtractor {
    def apply(s: String) = newTermName(s)
    def unapply(name: TermName): Option[String] = Some(name.toString)
  }

  final class TypeName(index0: Int, len0: Int, val next: TypeName, str0: String) extends Name(index0, len0, str0) with TypeNameApi {
    type ThisNameType = TypeName
    protected[this] def thisName: TypeName = this

    def isTermName: Boolean = false
    def isTypeName: Boolean = true
    def toTermName: TermName = {
      def body = {
        val n = store.termOf(this)
        assert (n ne null, s"TypeName $this is missing its correspondent")
        n
      }
      if (synchronizeNames) nameLock.synchronized(body) else body
    }
    def toTypeName: TypeName = this
    def newName(str: String): TypeName = newTypeName(str)
    def companionName: TermName = toTermName
    def subName(from: Int, to: Int): TypeName = {
      val sub = if (synchronizeNames) nameLock.synchronized(store.subName(this, from, to)) else store.subName(this, from, to)
      sub.toTypeName
    }

    def nameKind = "type"
    override def decode = if (nameDebug) super.decode + "!" else super.decode
  }

  implicit val TypeNameTag: ClassTag[TypeName] = ClassTag[TypeName](classOf[TypeName])

  object TypeName extends TypeNameExtractor {
    def apply(s: String) = newTypeName(s)
    def unapply(name: TypeName): Option[String] = Some(name.toString)
  }
}

/** Strings and ids of names, for sharing between universes. It holds its strings weakly: the universes' names keep them
 *  alive, so a string lasts as long as some universe has the name, and then its id is forgotten and a later universe
 *  gets a new one. That is safe as universes only compare ids of names that they hold.
 */
private[internal] final class SharedNameTable {
  import SharedNameTable.{Entry, EntryKey}
  private[this] val entries = new java.util.concurrent.ConcurrentHashMap[AnyRef, Entry](0x8000)
  private[this] val cleared = new java.lang.ref.ReferenceQueue[String]
  private[this] val nextId = new java.util.concurrent.atomic.AtomicInteger

  /** The current entry for `s`. A string that is about to go may leave the entry without one, so callers check. */
  def intern(s: String): Entry = {
    val found = entries.get(new EntryKey(s))
    if ((found ne null) && (found.get ne null)) found
    else {
      expunge()
      val entry = new Entry(s, nextId.getAndIncrement(), cleared)
      val raced = entries.putIfAbsent(entry, entry)
      if (raced eq null) entry else raced
    }
  }

  /** The string of the entry the probe describes, if it still has one. */
  def find(probe: SharedNameTable.Probe): String = {
    val found = entries.get(probe)
    if (found ne null) found.get else null
  }

  /** How many entries are held, which includes those cleared since the last new string. */
  def size: Int = entries.size

  private def expunge(): Unit = {
    var entry = cleared.poll()
    while (entry ne null) {
      entries.remove(entry)
      entry = cleared.poll()
    }
  }
}

private[internal] object SharedNameTable {
  val global = new SharedNameTable

  /** Its own key in the table. A cleared entry equals only itself, which is how it gets removed. */
  final class Entry(s: String, val id: Int, queue: java.lang.ref.ReferenceQueue[String]) extends java.lang.ref.WeakReference[String](s, queue) {
    private[this] val h = s.hashCode
    override def hashCode = h
    override def equals(other: Any) = other match {
      case that: Entry =>
        (this eq that) || {
          val a = get
          (a ne null) && a.equals(that.get)
        }
      case _ => false
    }
  }

  /** Looks up the entry for a string. */
  final class EntryKey(s: String) {
    override def hashCode = s.hashCode
    override def equals(other: Any) = other match {
      case that: Entry => s.equals(that.get)
      case _           => false
    }
  }

  /** A reusable key that looks up a name in either table without building its `String`, from chars, ASCII bytes or part
   *  of a `String`. `ConcurrentHashMap.get` hashes and calls `equals` on the key it is given, never the other way round,
   *  and the hash is `String.hashCode` of the contents. It is mutable, so each thread needs its own.
   */
  final class Probe {
    private[this] var chars: Array[Char] = null
    private[this] var bytes: Array[Byte] = null
    private[this] var region: String = null
    private[this] var sequence: CharSequence = null
    private[this] var offset = 0
    private[this] var len = 0
    private[this] var h = 0

    def setChars(cs: Array[Char], off: Int, length: Int, hash: Int): Unit = { chars = cs; offset = off; len = length; h = hash }
    def setAscii(bs: Array[Byte], off: Int, length: Int, hash: Int): Unit = { bytes = bs; offset = off; len = length; h = hash }
    def setRegion(s: String, off: Int, length: Int, hash: Int): Unit = { region = s; offset = off; len = length; h = hash }
    def setSequence(cs: CharSequence, length: Int, hash: Int): Unit = { sequence = cs; offset = 0; len = length; h = hash }
    def clear(): Unit = { chars = null; bytes = null; region = null; sequence = null }

    def newString(): String =
      if (chars ne null) new String(chars, offset, len)
      else if (bytes ne null) new String(bytes, offset, len, java.nio.charset.StandardCharsets.ISO_8859_1)
      else if (sequence ne null) sequence.toString
      else region.substring(offset, offset + len)

    private def matches(s: String): Boolean = (s ne null) && s.length == len && {
      if (region ne null) s.regionMatches(0, region, offset, len)
      else {
        var i = 0
        if (sequence ne null) while (i < len && s.charAt(i) == sequence.charAt(i)) i += 1
        else if (chars ne null) while (i < len && s.charAt(i) == chars(offset + i)) i += 1
        else while (i < len && s.charAt(i) == bytes(offset + i)) i += 1
        i == len
      }
    }
    override def hashCode = h
    override def equals(other: Any) = other match {
      case s: String   => matches(s)
      case e: Entry    => matches(e.get)
      case _           => false
    }
  }
}
