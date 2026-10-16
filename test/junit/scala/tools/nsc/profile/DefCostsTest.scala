package scala.tools.nsc.profile

import java.nio.file.Files

import org.junit.Assert._
import org.junit.Test

import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters._
import scala.tools.testkit.BytecodeTesting

class DefCostsTest {
  private val code =
    """package p
      |class A {
      |  def a: Int = b + implicitly[Ordering[Int]].hashCode
      |  def b = { def local = 1; local + 2 }
      |}
      |object O { val x = List(1).sorted }
      |""".stripMargin

  @Test def table(): Unit = {
    val out = Files.createTempFile("defcosts", ".tsv")
    try {
      val compiler = BytecodeTesting.newCompiler(extraArgs = s"-Yprofile-defs:$out")
      compiler.compileToBytes(code)
      val lines = Files.readAllLines(out).asScala.toList
      assertEquals("kind\tphase\tsymbol\tfile\tline\tchars\tcalls\tself_us\ttotal_us\tmax_self_us\tself_alloc", lines.head)
      val rows = lines.tail.map(_.split("\t", -1).toList)
      assertTrue(rows.forall(_.length == 11))
      def has(kind: String, phase: String, symbol: String) = rows.exists {
        case `kind` :: `phase` :: `symbol` :: _ => true
        case _ => false
      }
      def row(kind: String, phase: String, symbol: String) = rows.find {
        case `kind` :: `phase` :: `symbol` :: _ => true
        case _ => false
      }.getOrElse { fail(s"no row $kind $phase $symbol\n${lines.mkString("\n")}"); Nil }
      assertTrue(has("phase", "typer", "typer"))
      assertTrue(has("phase", "jvm", "jvm"))
      assertTrue(rows.exists(r => r.head == "unit" && r(1) == "typer"))
      for (d <- List("p.A", "p.A.a", "p.A.b", "p.O$", "p.O.x")) assertTrue(d, has("def", "", d))
      // b's inferred type is a completion of its own, with its position; a, b and A's constructor are completed from source
      val b = row("complete", "", "p.A.b")
      assertEquals("4", b(4))
      assertEquals("def b = { def local = 1; local + 2 }".length.toString, b(5))
      assertTrue(has("complete", "", "p.A.a"))
      // local definitions are part of the definition they are in
      assertFalse(rows.exists(_(2).contains("local")))
      assertTrue(has("implicit", "typer", "p.A.a"))
      assertTrue(rows.exists(r => r.head == "load" && r(2) == "scala.collection.immutable.List"))
      // retyped in erasure
      assertTrue(has("transform", "erasure", "p.A.a"))
      for (r <- rows) assertTrue(r.mkString("\t"), r(7).toLong <= r(8).toLong)
    } finally Files.delete(out)
  }

  @Test def listener(): Unit = {
    RecordingCostListener.events.clear()
    val compiler = BytecodeTesting.newCompiler(extraArgs = s"-Yprofile-defs-listener:${classOf[RecordingCostListener].getName}")
    compiler.compileToBytes(code)
    val events = RecordingCostListener.events.toList
    assertTrue(events.nonEmpty)
    // balanced, as a stack
    val stack = ListBuffer.empty[String]
    for ((enter, path) <- events) {
      if (enter) stack += path else assertEquals(stack.remove(stack.length - 1), path)
    }
    assertTrue(stack.isEmpty)
    assertEquals("finished", RecordingCostListener.finished)
    // b's completion while typing class A, the implicit search in a
    assertTrue(events.exists { case (e, p) => e && p.matches("phase:typer/unit:_/def:A(/def:a)?/complete:b") })
    assertTrue(events.exists { case (e, p) => e && p.endsWith("/def:a/implicit:implicit") })
  }
}

object RecordingCostListener {
  val events = ListBuffer.empty[(Boolean, String)]
  var finished = ""
}

class RecordingCostListener extends CostListener {
  private def path(frame: CostFrame): String = {
    val here = s"${frame.kind.label}:${if (frame.kind eq CostKind.Unit) "_" else frame.label}"
    if (frame.parent == null) here else path(frame.parent) + "/" + here
  }
  def enter(frame: CostFrame): Unit = RecordingCostListener.events += ((true, path(frame)))
  def exit(frame: CostFrame): Unit = RecordingCostListener.events += ((false, path(frame)))
  override def finished(): Unit = RecordingCostListener.finished = "finished"
}
