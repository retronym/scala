package scala.reflect.internal

import java.nio.charset.StandardCharsets.UTF_8

import org.junit.Assert.{assertThrows => _, _}
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.tools.nsc.symtab.SymbolTableForUnitTesting
import scala.tools.testkit.AssertUtil.assertThrows

/** Each name table against the behaviour of `String`, on ASCII, Latin-1, CJK and surrogate pairs. */
@RunWith(classOf[JUnit4])
class NameStoreTest {
  private val kinds = List("chars", "cached", "string", "bytes", "shared")

  private class Table(kind: String) extends SymbolTableForUnitTesting {
    override protected def nameTableKind: String = kind
  }

  private val samples = List(
    "", "a", "abc", "abcabc", "toString", "$colon$colon", "foo_=", "foo$bar$baz",
    "café", "é", "名前", "a名b前c", "abc名", "名abc",
    "😀", "x😀y", "\u0000", "a\u0000b")
  private val needles = List("a", "b", "abc", "$", "é", "名", "\ud83d", "\ude00", "zzz")

  private def forEachTable(f: (String, Table) => Unit): Unit = kinds.foreach(k => f(k, new Table(k)))

  @Test def contentsMatchString(): Unit = forEachTable { (kind, t) =>
    for (s <- samples) {
      val n = t.newTermName(s)
      val m = s"$kind '$s'"
      assertEquals(m, s, n.toString)
      assertEquals(m, s, n.toTypeName.toString)
      assertEquals(m, s.length, n.length)
      assertEquals(m, s, new String(n.toChars))
      for (i <- 0 until s.length) assertEquals(m, s.charAt(i), n.charAt(i))
      assertEquals(m, s, new java.lang.StringBuilder().append(n: CharSequence).toString)
      val sb = new StringBuilder
      n.appendTo(sb, 0, s.length)
      assertEquals(m, s, sb.toString)
      val sbuf = new java.lang.StringBuffer
      n.appendTo(sbuf, 0, s.length)
      assertEquals(m, s, sbuf.toString)
      assertArrayEquals(m, scala.io.Codec.toUTF8(s), n.toUTF8)
    }
  }

  @Test def searchesMatchString(): Unit = forEachTable { (kind, t) =>
    for (s <- samples; x <- needles; other = t.newTermName(x)) {
      val n = t.newTermName(s)
      val m = s"$kind '$s' '$x'"
      assertEquals(m, s.indexOf(x), n.indexOf(x))
      assertEquals(m, s.lastIndexOf(x), n.lastIndexOf(x))
      assertEquals(m, s.startsWith(x), n.startsWith(x))
      assertEquals(m, s.startsWith(x), n.startsWith(other))
      assertEquals(m, s.endsWith(x), n.endsWith(x))
      assertEquals(m, s.endsWith(x), n.endsWith(other))
      assertEquals(m, s.contains(x), n.containsName(other))
      val c = x.charAt(0)
      assertEquals(m, s.indexOf(c), n.indexOf(c))
      assertEquals(m, s.lastIndexOf(c), n.lastIndexOf(c))
      assertEquals(m, s.indexOf(c) >= 0, n.containsChar(c))
      for (from <- 0 to s.length) {
        assertEquals(m, s.startsWith(x, from), n.startsWith(other, from))
        assertEquals(m, s.startsWith(x, from), n.startsWith(x, from))
        assertEquals(m, s.indexOf(c, from), n.indexOf(c, from))
        assertEquals(m, s.substring(0, from).endsWith(x), n.endsWith(x, from))
        assertEquals(m, s.substring(0, from).endsWith(x), n.endsWith(other, from))
      }
    }
  }

  @Test def everyEntryFindsTheSameName(): Unit = forEachTable { (kind, t) =>
    for (s <- samples) {
      val n = t.newTermName(s)
      val m = s"$kind '$s'"
      assertSame(m, n, t.newTermName(s.toCharArray))
      assertSame(m, n, t.newTermNameHashed((s + "zz").toCharArray, s.length, s.hashCode))
      assertSame(m, n, t.newTermName(("xy" + s + "z").toCharArray, 2, s.length))
      assertSame(m, n, t.newTermNameCached(s))
      val bytes = s.getBytes(UTF_8)
      if (new String(bytes, UTF_8) == s) assertSame(m, n, t.newTermName(bytes, 0, bytes.length))
      assertSame(m, n, n.toTypeName.toTermName)
      assertSame(m, n.toTypeName, t.newTypeName(s))
      assertEquals(m, n.id, n.toTypeName.id)
      assertSame(m, n.toTypeName, t.lookupTypeName(s.toCharArray))
    }
  }

  @Test def aHashedEntryEntersANewName(): Unit = forEachTable { (kind, t) =>
    for (s <- List("enteredWithAHash", "\u540dWithAHash", "")) {
      val n = t.newTermNameHashed(s.toCharArray, s.length, s.hashCode)
      assertEquals(kind, s, n.toString)
      assertSame(kind, n, t.newTermName(s))
    }
  }

  @Test def subNamesAreTheSameNames(): Unit = forEachTable { (kind, t) =>
    for (s <- samples; from <- 0 to s.length; to <- from to s.length) {
      val n = t.newTermName(s)
      val sub = s.substring(from, to)
      val m = s"$kind '$s' $from $to"
      val first = n.subName(from, to)
      assertEquals(m, sub, first.toString)
      assertSame(m, t.newTermName(sub), first)
      assertSame(m, first.toTypeName, n.toTypeName.subName(from, to))
    }
  }

  @Test def subNameEnteredFirstIsFoundLater(): Unit = forEachTable { (kind, t) =>
    val n = t.newTermName("prefix名suffix")
    val ascii = n.subName(0, 6)
    val cjk = n.subName(6, 7)
    assertEquals(kind, "prefix", ascii.toString)
    assertSame(kind, ascii, t.newTermName("prefix"))
    assertSame(kind, cjk, t.newTermName("名"))
    assertEquals(kind, "prefixsuffix", ascii.append("suffix").toString)
  }

  @Test def idIsUniqueToAString(): Unit = forEachTable { (kind, t) =>
    val names = (samples ++ needles ++ (0 until 500).map("name" + _) ++ (0 until 500).map("名" + _)).distinct.map(t.newTermName(_: String))
    // A name may share its id with a prefix of it, in a range of the table, but then the lengths differ.
    assertEquals(kind, names.size, names.map(n => (n.id, n.length)).distinct.size)
    assertEquals(kind, names.toSet, t.allNames().filter(n => names.contains(n)).toSet)
  }

  @deprecated("tests the deprecated accessor", since = "")
  @Test def tableAccessFailsInformativelyWithoutACharTable(): Unit = {
    for (kind <- List("chars", "cached")) {
      val t = new Table(kind)
      val n = t.newTermName("abc")
      assertEquals(kind, "abc", new String(t.chrs, n.start, n.length))
    }
    for (kind <- List("string", "bytes", "shared")) {
      val t = new Table(kind)
      val n = t.newTermName("abc")
      for (what <- List[(String, () => Any)](("Name#start", () => n.start), ("Names#chrs", () => t.chrs), ("Names#chrs", () => t.chrs = new Array[Char](1)))) {
        assertThrows[UnsupportedOperationException](what._2(), m => m.contains(what._1) && m.contains(s"-Yname-table:$kind"))
      }
    }
  }

  @Test def growsPastItsFirstCapacity(): Unit = forEachTable { (kind, t) =>
    val long = "x" * 100000
    val names = (0 until 5).map(i => t.newTermName(s"$i$long"))
    for ((n, i) <- names.zipWithIndex) assertEquals(kind, s"$i$long", n.toString)
    val wide = "名" * 100000
    val wides = (0 until 5).map(i => t.newTermName(s"$i$wide"))
    for ((n, i) <- wides.zipWithIndex) assertEquals(kind, s"$i$wide", n.toString)
  }

  @Test def sharedTablesAgreeOnStrings(): Unit = {
    val (a, b) = (new Table("shared"), new Table("shared"))
    val (x, y) = (a.newTermName("sharedBetweenTables"), b.newTermName("sharedBetweenTables".toCharArray))
    assertNotSame(x, y)
    assertSame(x.toString, y.toString)
    assertSame(x.subName(0, 6).toString, b.newTermName("shared").toString)
  }

  @Test def idsAreDenseInATable(): Unit = forEachTable { (kind, t) =>
    if (kind == "shared" || kind == "string") {
      new Table(kind).newTermName("enteredElsewhere")
      val n = t.newTermName("enteredHere")
      assertEquals(kind, t.nameTableSize - 1, n.id)
    }
  }

  private def collectUntil(done: () => Boolean): Unit = {
    var i = 0
    while (i < 50 && !done()) { System.gc(); Thread.sleep(10); i += 1 }
  }

  @Test def sharedTableForgetsStringsNoOneHolds(): Unit = {
    val table = new SharedNameTable
    val kept = new String("kept")
    assertSame(kept, table.intern(kept))
    var dropped = new String("dropped")
    assertSame(dropped, table.intern(dropped))
    assertEquals(2, table.size)
    val ref = new java.lang.ref.WeakReference(dropped)
    dropped = null
    collectUntil(() => ref.get eq null)
    assertNull(ref.get)
    // Interning a new string removes the cleared entry
    val another = new String("another")
    table.intern(another)
    assertEquals(2, table.size)
    assertSame(kept, table.intern(new String("kept")))
    assertSame(another, table.intern(new String("another")))
  }

  @Test def sharedTableKeepsStringsAUniverseHolds(): Unit = {
    val a = new Table("shared")
    val n = a.newTermName(new String("heldByAUniverse"))
    collectUntil(() => { System.gc(); true })
    val b = new Table("shared")
    assertSame(n.toString, b.newTermName(new String("heldByAUniverse")).toString)
  }

  @Test def sharedTableIsSafeAcrossConcurrentCompilers(): Unit = {
    val strings = (0 until 20000).map(i => s"concurrent${i % 5000}").toArray
    val threads = 8
    val tables = Array.fill(threads)(new Table("shared"))
    val results = new Array[Array[String]](threads)
    val ts = (0 until threads).map { t =>
      new Thread(() => {
        val table = tables(t)
        results(t) = strings.map(s => table.newTermName(s.toCharArray).toString)
      })
    }
    ts.foreach(_.start()); ts.foreach(_.join())
    for (r <- results) assertArrayEquals(strings.asInstanceOf[Array[AnyRef]], r.asInstanceOf[Array[AnyRef]])
    for (s <- strings.take(5000))
      assertEquals(1, tables.map(_.newTermName(s).toString: AnyRef).map(System.identityHashCode).distinct.length)
  }

  @Test def oneTableIsSafeAcrossThreads(): Unit = {
    val table = new Table("shared") { override def synchronizeNames = true }
    val strings = (0 until 5000).map(i => s"sameTable$i").toArray
    val seen = new Array[Array[AnyRef]](8)
    val ts = (0 until 8).map { t =>
      new Thread(() => seen(t) = strings.map(s => table.newTermName(s.toCharArray): AnyRef))
    }
    ts.foreach(_.start()); ts.foreach(_.join())
    for (r <- seen; i <- strings.indices) assertSame(seen(0)(i), r(i))
  }
}
