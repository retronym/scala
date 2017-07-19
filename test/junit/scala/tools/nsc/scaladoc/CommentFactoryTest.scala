package scala.tools.nsc.scaladoc

import org.junit.Assert.assertEquals
import org.junit.{Assert, Test}

import scala.tools.nsc.doc.base.comment._
import scala.tools.nsc.doc.model._
import scala.tools.nsc.doc.model.diagram._
import scala.tools.nsc.{Global, doc}

class Factory(val g: Global, val s: doc.Settings)
  extends doc.model.ModelFactory(g, s) {
  thisFactory: Factory
  with ModelFactoryImplicitSupport
  with ModelFactoryTypeSupport
  with DiagramFactory
  with CommentFactory
  with doc.model.TreeFactory
  with MemberLookup =>

  def strip(c: Comment): Option[Inline] = {
    c.body match {
      case Body(List(Paragraph(Chain(List(Summary(inner)))))) => Some(inner)
      case _ => None
    }
  }

  def getComment(s: String): Comment =
    parse(s, "", scala.tools.nsc.util.NoPosition, null)

  def parseComment(s: String): Option[Inline] =
    strip(getComment(s))

  def createBody(s: String) =
    parse(s, "", scala.tools.nsc.util.NoPosition, null).body
}

class CommentFactoryTest {
  val factory = {
    val settings = new doc.Settings((str: String) => {})
    val reporter = new scala.tools.nsc.reporters.ConsoleReporter(settings)
    val g = new Global(settings, reporter)
    (new Factory(g, settings)
      with ModelFactoryImplicitSupport
      with ModelFactoryTypeSupport
      with DiagramFactory
      with CommentFactory
      with doc.model.TreeFactory
      with MemberLookup)
  }

  def parse(src: String, dst: Inline): Unit = {
    assertEquals(Some(dst), factory.parseComment(src))
  }

  @Test def parse1(): Unit = parse(
      "/** One two three */",
      Text("One two three")
  )
  @Test def parse2(): Unit = parse(
    "/** One `two` three */",
    Chain(List(Text("One "), Monospace(Text("two")), Text(" three")))
  )

  @Test def parse3(): Unit = parse(
      """
/** One two
  * three */""",
      Text("One two\nthree")
  )
  @Test def parse4(): Unit = parse(
      """
/** One `two`
  * three */""",
      Chain(List(Text("One "), Monospace(Text("two")), Text("\n"), Text("three")))
  )

  @Test def parse5(): Unit = parse(
      """
/** One `two`
 *  three */""",
      Chain(List(Text("One "), Monospace(Text("two")), Text("\n"), Text(" three")))
  )

  @Test def parse6(): Unit = parse(
      """
/** One
  * `two` three */""",
      Chain(List(Text("One"), Text("\n"), Monospace(Text("two")), Text(" three")))
  )

  @Test def t4361_caretPair(): Unit = parse(
      """
/**
 * hello ^world^ */""",
      Chain(List(Text("hello "), Superscript(Text("world"))))
  )

  @Test def t4361_caret(): Unit = parse(
      """
/**
 * <pre>
 * hello ^world
 * </pre>
 *
 */""",
      Chain(List(Text(""), Text("\n"),


                 HtmlTag("<pre>\nhello ^world\n</pre>")))
  )

  @Test def t4366_body(): Unit = {
    val body = factory.createBody(
      """
 /**
  * <strong><code>foo</code> has been deprecated and will be removed in a future version. Please call <code>bar</code> instead.</strong>
  */
      """
    )
    assertEquals(Body(List(Paragraph(Chain(List(
      Summary(Chain(List(HtmlTag("<strong><code>foo</code> has been deprecated and will be removed in a future version. Please call <code>bar</code> instead.</strong>"), Text("\n"), Text(""))))
    )))))
    , body)
  }

  @Test def t4366_summary(): Unit = {
    val body = factory.createBody(
      """
 /**
  * <strong><code>foo</code> has been deprecated and will be removed in a future version. Please call <code>bar</code> instead.</strong>
  */
      """
    )
    assertEquals(Some(Chain(List(HtmlTag("<strong><code>foo</code> has been deprecated and will be removed in a future version. Please call <code>bar</code> instead.</strong>"), Text("\n"), Text(""))))
      , body.summary)
  }

  @Test def t4358_body(): Unit = {
    factory.createBody(
      """
 /**
   * Implicit conversion that invokes the <code>expect</code> method on the <code>EasyMock</code> companion object (<em>i.e.</em>, the
   * static <code>expect</code> method in Java class <code>org.easymock.EasyMock</code>).
  */
      """
    ) match {
      case Body(List(Paragraph(Chain(List(Summary(Chain(List(Chain(List(
        Text("Implicit conversion that invokes the "),
        HtmlTag("<code>expect</code>"),
        Text(" method on the "),
        HtmlTag("<code>EasyMock</code>"),
        Text(" companion object ("),
        HtmlTag("<em>i.e.</em>"),
        Text(", the\nstatic "),
        HtmlTag("<code>expect</code>"),
        Text(" method in Java class "),
        HtmlTag("<code>org.easymock.EasyMock</code>"),
        Text(")")
      )), Text(".")))), Text("\n")))))) =>
          // okay
      case other => {
        Assert.fail(other.toString)
      }
    }
  }

  @Test def EmptyParameterTextShouldBeEmpty(): Unit = {
    // used to fail with
    // body == Body(List(Paragraph(Chain(List(Summary(Text('\n')))))))
    factory.getComment(
      """
/**
  * @deprecated
  */
      """).deprecated match {
      case Some(Body(l)) if l.isEmpty => // okay
      case other =>
        Assert.fail(other.toString)
    }
  }
}
