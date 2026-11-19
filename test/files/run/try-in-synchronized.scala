//> using options -Wconf:cat=deprecation:s
// A `try` with a `catch` in the body of `synchronized` is not lifted to a method
// if the `synchronized` expression itself is evaluated with an empty stack.
// The class must verify, and the monitor must be released on every path.
class C {
  private val lock = new Object
  def parse(s: String): Int = s.toInt
  def id(s: String): String = s
  var field = 0

  def syncStat(s: String): Int = {
    var count = 0
    lock.synchronized { try { count += 1; parse(s) } catch { case _: NumberFormatException => count -= 1 } }
    count
  }
  def syncExpr(s: String): Int = lock.synchronized { try parse(s) catch { case _: NumberFormatException => -1 } }
  def syncVal(s: String): Int = { val r = lock.synchronized { try parse(s) catch { case _: NumberFormatException => -1 } }; r + 1 }
  def syncNested(s: String): Int = lock.synchronized { lock.synchronized { try parse(s) catch { case _: NumberFormatException => -1 } } }
  def syncBlock(s: String): Int = lock.synchronized { val r = try parse(s) catch { case _: NumberFormatException => -1 }; r + 1 }
  def syncQual(s: String): Int = (try { parse(s); lock } catch { case _: NumberFormatException => this }).synchronized { s.length }
  def syncReturn(s: String): Int = { lock.synchronized { try return parse(s) catch { case _: NumberFormatException => () } }; -1 }
  def syncFinally(s: String): Int = lock.synchronized { try parse(s) catch { case _: NumberFormatException => -1 } finally field += 1 }
  def syncRethrow(s: String): Int = lock.synchronized { try parse(s) catch { case e: NumberFormatException => throw new IllegalStateException(e) } }
  def syncLong(s: String): Long = 1L + { val r = lock.synchronized { try parse(s).toLong catch { case _: NumberFormatException => -1L } }; r }
  def syncThis(s: String): Int = synchronized { try parse(s) catch { case _: NumberFormatException => -1 } }
  def syncLambda(s: String): Int = { val f = () => lock.synchronized { try parse(s) catch { case _: NumberFormatException => -1 } }; f() }

  def syncArg(s: String): String = id(lock.synchronized { try { parse(s); s } catch { case _: NumberFormatException => "" } })
  def syncOperand(s: String): Int = 1 + lock.synchronized { try parse(s) catch { case _: NumberFormatException => -1 } }
  def syncInArg(s: String): String = lock.synchronized { id(try { parse(s); s } catch { case _: NumberFormatException => "" }) }
  def assignField(s: String): Int = { field = try parse(s) catch { case _: NumberFormatException => -1 }; field }
  def assignLocal(s: String): Int = { var r = 0; r = try parse(s) catch { case _: NumberFormatException => -1 }; r }
  def assignCaptured(s: String): Int = { var r = 0; r = try parse(s) catch { case _: NumberFormatException => -1 }; val g = () => r; g() }

  // the receiver of a String concatenation is evaluated after the StringBuilder is created
  def concatSync(s: String): String = lock.synchronized { try { parse(s); s } catch { case _: NumberFormatException => "" } } + s
  def concatSyncToString(s: String): String = lock.synchronized { try parse(s) catch { case _: NumberFormatException => -1 } }.toString + s
  def concatTry(s: String): String = (try parse(s) catch { case _: NumberFormatException => -1 }).toString + s
  def concatIntRecv(s: String): String = (try parse(s) catch { case _: NumberFormatException => -1 }) + s
  def concatNested(s: String): String = (lock.synchronized { try { parse(s); s } catch { case _: NumberFormatException => "" } } + s) + s
  def concatArg(s: String): String = s + lock.synchronized { try parse(s) catch { case _: NumberFormatException => -1 } }
  def arrayStore(s: String): Int = { val a = new Array[Int](1); a(0) = lock.synchronized { try parse(s) catch { case _: NumberFormatException => -1 } }; a(0) }
  def newArg(s: String): String = new Some(lock.synchronized { try { parse(s); s } catch { case _: NumberFormatException => "" } }).value

  def locked: Boolean = Thread.holdsLock(lock) || Thread.holdsLock(this)
}

object Test {
  def main(args: Array[String]): Unit = {
    val c = new C
    def check[T](name: String)(f: String => T): Unit = {
      def run(s: String) = try f(s) catch { case e: IllegalStateException => "ISE" }
      println(s"$name: ${run("1")} ${run("x")}")
      assert(!c.locked, name)
    }
    check("syncStat")(c.syncStat)
    check("syncExpr")(c.syncExpr)
    check("syncVal")(c.syncVal)
    check("syncNested")(c.syncNested)
    check("syncBlock")(c.syncBlock)
    check("syncQual")(c.syncQual)
    check("syncReturn")(c.syncReturn)
    check("syncFinally")(c.syncFinally)
    check("syncRethrow")(c.syncRethrow)
    check("syncLong")(c.syncLong)
    check("syncThis")(c.syncThis)
    check("syncLambda")(c.syncLambda)
    check("syncArg")(c.syncArg)
    check("syncOperand")(c.syncOperand)
    check("syncInArg")(c.syncInArg)
    check("assignField")(c.assignField)
    check("assignLocal")(c.assignLocal)
    check("assignCaptured")(c.assignCaptured)
    check("concatSync")(c.concatSync)
    check("concatSyncToString")(c.concatSyncToString)
    check("concatTry")(c.concatTry)
    check("concatIntRecv")(c.concatIntRecv)
    check("concatNested")(c.concatNested)
    check("concatArg")(c.concatArg)
    check("arrayStore")(c.arrayStore)
    check("newArg")(c.newArg)
    println(c.field)
  }
}
