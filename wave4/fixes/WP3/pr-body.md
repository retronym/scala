A value class result that erasure boxes as a whole, around an expression holding a `try`/`catch`, compiles to a method that fails verification when its class is loaded, and crashes the compiler under `-opt:local`:

```scala
case class Meters(value: Double) extends AnyVal

// VerifyError: Inconsistent stackmap frames (AnalyzerException under -opt:local)
def parseAll(xs: List[Any]): List[Meters] = xs.collect { case x => x match {
  case d: Double => Meters(d)
  case s: String => try Meters(s.toDouble) catch { case _: NumberFormatException => Meters(0) }
} }

def a(s: String): Any = (try Meters(s.toDouble) catch { case _: NumberFormatException => Meters(0) }): Meters
```

With a lambda inside the `try`, the back end crashes instead (`Error while emitting`, an index out of bounds). Both should run. Scala 3 compiles them correctly, and binding the `try` to a `val` first works.

A `try` with a `catch` can only be emitted where the operand stack is empty, because the JVM clears the stack on entry to a handler; uncurry moves a `try` in any other position into a local method. Here uncurry sees an empty stack: in the first example the `try` is the argument of the inner match's end label, in the second it is the whole result of the method. Erasure then adds a box around it. The partial function's `applyOrElse` returns the erased result type `Object`, and the outer match is typed at `Meters`, so the whole inner block becomes the argument of `new Meters(...)`; the ascription does the same in a method of result type `Any`. `NEW` and `DUP` are then on the stack when the handler is entered. A plain lambda, a method returning the value class, an argument position (uncurry lifts there) and a match that becomes a switch (boxed per case) are not affected.

The nested-match route is a regression in 2.13.9 and 2.12.17 from scala/scala#10028, which made erasure adapt a `Block(_, LabelDef)` from outside instead of pushing the box into the label, to keep the pattern matcher's tree shapes for the back end. The ascription route is older (2.11.12 and 2.13.8 fail too).

The fix is in erasure's `box`: when the expression to be boxed into a derived value class has a `try` with a `catch` that is evaluated in the current method (not inside a nested method, class or function), it is stored in a fresh local first and the local is boxed. The bytecode of every other box is unchanged, and a method whose bytecode changes was rejected by the verifier before, unless each of its handlers leaves the method (by `throw` or `return`). Uncurry cannot make this decision, since whether a box appears depends on the erased type of the enclosing position. A `try`/`finally` without `catch` is left as it is, as uncurry does, because its handler rethrows and does not continue on the emptied stack. For a value class over `Unit`, the expression is evaluated as a statement before the box.

retronym/scala#605 (CF3) is the same family, a `try` that uncurry leaves in place and a later phase puts behind a non-empty stack, for receivers that erasure rewrites into `ScalaRunTime` calls and singleton type tests; it does not cover this case. Related tickets: scala/bug#12550 (a value class receiver, which uncurry now lifts) and scala/bug#12993 (the String concatenation case, still open).

Found in the fourth wave of the coordinated code review of 2.13.x (finding WP3). Prepared with Claude.
