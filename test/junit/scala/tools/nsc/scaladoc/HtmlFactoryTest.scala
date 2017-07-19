package scala.tools.nsc.scaladoc


import org.junit.Assert.{assertEquals, assertFalse, assertTrue}
import org.junit.Test

import scala.collection.mutable
import scala.tools.nsc.ScalaDocReporter
import scala.tools.nsc.doc.html.HtmlFactory
import scala.tools.nsc.doc.{DocFactory, Settings}
import scala.xml.{Node, NodeSeq}

object XMLUtil {
  import scala.xml._

  def stripGroup(seq: Node): Node = {
    seq match {
      case group: Group => {
        <div class="group">{ group.nodes.map(stripGroup _) }</div>
      }
      case e: Elem => {
        val child = e.child.map(stripGroup _)
        Elem(e.prefix, e.label, e.attributes, e.scope, child : _*)
      }
      case _ => seq
    }
  }
}

class BaseHtmlFactoryTest {
  def createFactory = {
    val settings = new Settings({Console.err.println(_)})
    settings.scaladocQuietRun = true
    settings.nowarn.value = true
    SettingsUtil.configureClassAndSourcePath(settings)
    settings.docAuthor.value = true

    val reporter = new scala.tools.nsc.reporters.ConsoleReporter(settings)
    new DocFactory(reporter, settings)
  }

  def createTemplates(basename: String): collection.Map[String, NodeSeq] = {
    val result = mutable.Map[String, NodeSeq]()

    val path: String = SettingsUtil.checkoutRoot.resolve(SettingsUtil.RESOURCES).resolve(basename).toAbsolutePath.toString
    createFactory.makeUniverse(Left(List(path))) match {
      case Some(universe) => {
        new HtmlFactory(universe, new ScalaDocReporter(universe.settings)).writeTemplates((page) => {
          result += (page.absoluteLinkTo(page.path) -> page.body)
        })
      }
      case _ =>
    }

    result
  }

  def createTemplate(scala: String): Node = {
    val html = scala.stripSuffix(".scala") + ".html"
    createTemplate(scala, html)
  }

  def createTemplate(scala: String, htmlfile: String): Node = {
    val html = scala.stripSuffix(".scala") + ".html"
    createTemplates(scala)(htmlfile) match {
      case n: Node => n
      case _ => sys.error(s"Could not find $htmlfile as a Node in result")
    }
  }

  /**
    * This tests the text without the markup - ex:
    *
    * <h4 class="signature">
    *  <span class="modifier_kind">
    *    <span class="modifier">implicit</span>
    *    <span class="kind">def</span>
    *  </span>
    *  <span class="symbol">
    *    <span class="name">test</span><span class="params">()</span><span class="result">: <span name="scala.Int" class="extype">Int</span></span>
    *  </span>
    *  </h4>
    *
    * becomes:
    *
    *  implicit def test(): Int
    *
    * and is required to contain the text in the given checks
    *
    * NOTE: Comparison is done ignoring all whitespace
    */
  def checkText(scalaFile: String, debug: Boolean = true)(checks: (Option[String], String, Boolean)*): Unit = {
    val htmlFile = scalaFile.stripSuffix(".scala") + ".html"
    val htmlAllFiles = createTemplates(scalaFile)
    var result = true

    for ((fileHint, check, expected) <- checks) {
      // resolve the file to be checked
      val fileName = fileHint match {
        case Some(file) =>
          if (file endsWith ".html")
            file
          else
            file + ".html"
        case None =>
          htmlFile
      }
      val fileTextPretty = htmlAllFiles(fileName).text.replace('→',' ').replaceAll("\\s+"," ")
      val fileText = fileTextPretty.replaceAll(" ", "")

      val checkTextPretty = check.replace('→',' ').replaceAll("\\s+"," ")
      val checkText = checkTextPretty.replaceAll(" ", "")

      val checkValue = fileText.contains(checkText) == expected
      if (debug && (!checkValue)) {
        Console.err.println("")
        Console.err.println("HTML Check failed for resource file " + scalaFile + ":")
        Console.err.println("Could not match: \n" + checkTextPretty)
        Console.err.println("In the extracted HTML text: \n" + fileTextPretty)
        Console.err.println("NOTE: The whitespaces are eliminated before matching!")
        Console.err.println("")
      }
      result &&= checkValue
    }

    assertTrue(result)
  }

  def shortComments(root: scala.xml.Node) =
    XMLUtil.stripGroup(root).descendant.flatMap {
      case e: scala.xml.Elem => {
        if (e.attribute("class").toString.contains("shortcomment")) {
          Some(e)
        } else {
          None
        }
      }
      case _ => None
    }
}

class HtmlFactoryTest extends BaseHtmlFactoryTest {

  @Test def Trac_3790(): Unit = {
    val node = createTemplate("Trac3790.scala")
    val comments = shortComments(node)

    assertTrue(comments.exists { _.toString.contains(">A lazy String\n</p>") })
    assertTrue(comments.exists { _.toString.contains(">A non-lazy String\n</p>") })
  }

  @Test def Trac_4306(): Unit = {
    val files = createTemplates("Trac4306.scala")
    files.contains("com/example/trac4306/foo/package$$Bar.html")
  }

  @Test def Trac_4366(): Unit = {
    val node = createTemplate("Trac4366.scala")
    assertTrue(shortComments(node).exists { n => {
      val str = n.toString
      str.contains("<code>foo</code>") && str.contains("</strong>")
    }})
  }

  @Test def Trac_4358(): Unit = {
    val node = createTemplate("Trac4358.scala")
    assertFalse(shortComments(node).exists {
      _.toString.contains("<em>i.</em>")
    })
  }

  @Test def Trac_4180(): Unit = {
    createTemplate("Trac4180.scala")
  }

  @Test def Trac_4372(): Unit = {
    val node = createTemplate("Trac4372.scala")
    val html = node.toString
    assertTrue(html.contains("<span title=\"gt4s: $plus$colon\" class=\"name\">+:</span>"))
    assertTrue(html.contains("<span title=\"gt4s: $minus$colon\" class=\"name\">-:</span>"))
    assertTrue(html.contains("""<span class="params">(<span name="n">n: <span class="extype" name="scala.Int">Int</span></span>)</span><span class="result">: <span class="extype" name="scala.Int">Int</span></span>"""))
  }

  @Test def Trac_4374_public(): Unit = {
    val files = createTemplates("Trac4374.scala")
    files("WithPublic.html") match {
      case node: scala.xml.Node =>
        val s = node.toString
        assertTrue(s.contains("""href="WithPublic$.html""""))
        assertTrue(files.get("WithPublic$.html").isDefined)
      case _ => ???
    }
  }

  @Test def Trac_4374_private(): Unit = {
    val files = createTemplates("Trac4374.scala")
    files("WithPrivate.html") match {
      case node: scala.xml.Node =>
        val s = node.toString
        assertFalse(s.contains("""href="WithPrivate$.html""""))
        assertTrue(files.get("WithPrivate$.html").isEmpty)
      case _ => ???
    }
  }

  @Test def Trac_4325_files(): Unit = {
    val files = createTemplates("Trac4325.scala")

    assertTrue(files.get("WithSynthetic.html").isDefined)
    assertTrue(files.get("WithSynthetic$.html").isEmpty)
    assertTrue(files.get("WithObject.html").isDefined)
    assertTrue(files.get("WithObject$.html").isDefined)
  }

  @Test def Trac_4325_DontLinkToSyntheticCompanion(): Unit = {
    val node = createTemplate("Trac4325.scala", "WithSynthetic.html")

    val s = node.toString
    assertFalse(s.contains("""href="WithSynthetic$.html""""))
  }

  @Test def Trac_4325_LinkToCompanion(): Unit = {
    val node = createTemplate("Trac4325.scala", "WithObject.html")
    val s = node.toString
    assertTrue(s.contains("""href="WithObject$.html""""))
  }

  @Test def Trac_4420_NoWhitespaceAtEndOfLine(): Unit = {
    val node = createTemplate("Trac4420.scala", "TestA.html")
    val s = node.toString
    assertTrue(s.contains("""See YYY for more details"""))
  }

  @Test def Trac_4289(): Unit = {
    val node = createTemplate("Trac4289.scala", "Subclass.html")
    assertTrue(node.toString.contains {
      """<dt>returns</dt><dd class="cmt"><p>123</p></dd>"""
    })
  }

  @Test def Trac_4409(): Unit = {
    val node = createTemplate("Trac4409.scala")
    assertFalse(node.toString.contains("""<div class="block"><ol>since"""))
  }

  @Test def Trac_4452(): Unit = {
    val node = createTemplate("Trac4452.scala")
    assertFalse(node.toString.contains(">*"))
  }

  @Test def t4421(): Unit = {
    val node = createTemplate("t4421.scala")

    val html = node.toString
    assertTrue(html.contains(">Example:"))
    assertTrue(html.contains(">Note<"))
  }

  @Test def t4589(): Unit = {
    val node = createTemplate("t4589.scala")
    val html = node.toString
    assertTrue(html.contains(">x0123456789: <"))
    assertTrue(html.contains(">x012345678901234567890123456789: <"))
  }

  @Test def t4714_ShouldDecodeSymbolicTypeAliasName(): Unit = {
    val node = createTemplate("t4715.scala")
    val html = node.toString
    assertTrue(html.contains(">:+:<"))
  }

  @Test def t4287_DefaultArgumentOfSynthesizedConstructor(): Unit = {
    val files = createTemplates("t4287.scala")

    files("ClassWithSugar.html") match {
      case node: scala.xml.Node => {
        assertTrue(node.toString.contains(">123<"))
      }
      case _ => ???
    }
  }

  @Test def t4507_DefaultArgumentsOfSynthesizedConstructor(): Unit = {
    val node = createTemplate("t4507.scala")
    assertFalse(node.toString.contains("<li>returns silently when evaluating true and true</li>"))
  }

  @Test def t4898_UseCasesAndLinksShouldNotCrashScaladoc(): Unit = {
    createTemplate("t4898.scala")
  }

  @Test def t5054_UseCasesShouldOverrideTheirOriginalMembers(): Unit =
     checkText("t5054_q1.scala")(
       (None,"""def test(): Int""", true)
       //Disabled because the full signature is now displayed
       //(None, """def test(implicit lost: Int): Int""", false)
     )

  @Test def t5054_UseCasesShouldKeepTheirFlags_FinalShouldNotBeLost(): Unit =
    checkText("t5054_q2.scala")((None, """final def test(): Int""", true))

  @Test def t5054_UseCaseShouldKeepTheirFlags_ImplicitShouldNotBeLost(): Unit =
    checkText("t5054_q3.scala")((None, """implicit def test(): Int""", true))

  @Test def t5054_UseCasesShouldKeepTheirFlags_realAbstractShouldNotBeLost(): Unit =
    checkText("t5054_q4.scala")((None, """abstract def test(): Int""", true))

  @Test def t5054_UseCasesShouldKeepTheirFlags_TraitsShouldNotBeAffected(): Unit =
    checkText("t5054_q5.scala")((None, """def test(): Int""", true))

  @Test def t5054_UseCasesShouldKeepTheirFlags_TraitsShouldNotBeAffected2(): Unit =
    checkText("t5054_q6.scala")((None, """abstract def test(): Int""", true))

  @Test def t5054_UseCaseIndividualSignatureTest(): Unit =
    checkText("t5054_q7.scala")(
      (None, """abstract def test2(explicit: Int): Int [use case] This takes the explicit value passed.""", true),
      (None, """abstract def test1(): Int [use case] This takes the implicit value in scope.""", true)
    )

  @Test def t5287_DisplayCorrectDefinitionClasses(): Unit =
    checkText("t5287.scala")(
      (None,
          """def method(): Int
           [use case] The usecase explanation
           [use case] The usecase explanation
           Definition Classes t5287 t5287_B t5287_A""", true)
    )      // the explanation appears twice, as small comment and full comment

  @Test def CommentInheritance_CorrectCommentInheritanceForOverriding(): Unit =
    checkText("implicit-inheritance-override.scala")(
      (Some("Base"),
       """def function[T](arg1: T, arg2: String): Double
          The base comment.
          The base comment. And another sentence...
          T the type of the first argument
          arg1 The T term comment
          arg2 The string comment
          returns The return comment
          """, true),
      (Some("DerivedA"),
       """def function[T](arg1: T, arg2: String): Double
          Overriding the comment, the params and returns comments should stay the same.
          Overriding the comment, the params and returns comments should stay the same.
          T the type of the first argument
          arg1 The T term comment
          arg2 The string comment
          returns The return comment
          """, true),
      (Some("DerivedB"),
       """def function[T](arg1: T, arg2: String): Double
          T the type of the first argument
          arg1 The overridden T term comment
          arg2 The overridden string comment
          returns The return comment
          """, true),
      (Some("DerivedC"),
       """def function[T](arg1: T, arg2: String): Double
          T the type of the first argument
          arg1 The T term comment
          arg2 The string comment
          returns The overridden return comment
          """, true),
      (Some("DerivedD"),
       """def function[T](arg1: T, arg2: String): Double
          T The overridden type parameter comment
          arg1 The T term comment
          arg2 The string comment
          returns The return comment
          """, true)
    )

  private def checkCommentInheritanceUseCase(useCaseFile: String): Unit =
    checkText("implicit-inheritance-usecase.scala")(
      (Some(useCaseFile),
        """def missing_arg[T](arg1: T): Double
            [use case]
            [use case]
            T The type parameter
            arg1 The T term comment
            returns The return comment
            """, true),
      (Some(useCaseFile),
        """def missing_targ(arg1: Int, arg2: String): Double
            [use case]
            [use case]
            arg1 The T term comment
            arg2 The string comment
            returns The return comment
            """, true),
      (Some(useCaseFile),
        """def overridden_arg1[T](implicit arg1: T, arg2: String): Double
            [use case]
            [use case]
            T The type parameter
            arg1 The overridden T term comment
            arg2 The string comment
            returns The return comment
            """, true),
      (Some(useCaseFile),
        """def overridden_targ[T](implicit arg1: T, arg2: String): Double
            [use case]
            [use case]
            T The overridden type parameter comment
            arg1 The T term comment
            arg2 The string comment
            returns The return comment
            """, true),
      (Some(useCaseFile),
        """def overridden_return[T](implicit arg1: T, arg2: String): Double
            [use case]
            [use case]
            T The type parameter
            arg1 The T term comment
            arg2 The string comment
            returns The overridden return comment
            """, true),
      (Some(useCaseFile),
        """def added_arg[T](implicit arg1: T, arg2: String, arg3: Float): Double
            [use case]
            [use case]
            T The type parameter
            arg1 The T term comment
            arg2 The string comment
            arg3 The added float comment
            returns The return comment
            """, true),
      (Some(useCaseFile),
        """def overridden_comment[T](implicit arg1: T, arg2: String): Double
            [use case] The overridden comment.
            [use case] The overridden comment.
            T The type parameter
            arg1 The T term comment
            arg2 The string comment
            returns The return comment
            """, true)
    )

  @Test def CommentInheritance_CorrectCommentInheritanceForUsecases_UseCaseInheritance(): Unit = checkCommentInheritanceUseCase("UseCaseInheritance")
  @Test def CommentInheritance_CorrectCommentInheritanceForUsecases_OverrideInheritance(): Unit = checkCommentInheritanceUseCase("UseCaseOverrideInheritance")

  @Test def CommentInheritance_CorrectExplicitInheritanceForOverride(): Unit =
  checkText("explicit-inheritance-override.scala")(
    (Some("InheritDocDerived"),
     """def function[T](arg1: T, arg2: String): Double
        Starting line
        Starting line
        The base comment. And another sentence...
        The base comment. And another sentence...
        Ending line
        Author: StartAuthor a Scala developer EndAuthor
          T       StartT the type of the first argument EndT
          arg1    Start1 The T term comment End1
          arg2    Start2 The string comment End2
          returns StartRet The return comment EndRet""", true),
    (Some("InheritDocDerived"),
     """Definition Classes InheritDocDerived → InheritDocBase
        Example:   StartExample function[Int](3, "something") EndExample
        Version    StartVer 0.0.2 EndVer
        Since      StartSince 0.0.1 EndSince
        Exceptions thrown
                   SomeException      StartEx if the function is not called with correct parameters EndEx
                   SomeOtherException StartSOE Should Warn <invalid inheritdoc annotation> EndSOE
        To do      StartTodo Call mom. And dad! EndTodo
        Note       StartNote Be careful! EndNote
        See also   StartSee The Manual EndSee
     """, true))

  @Test def CommentInheritance_CorrectExplicitInheritanceForUsecase(): Unit =
  checkText("explicit-inheritance-usecase.scala")(
    (Some("UseCaseInheritDoc"),
     """def function[T](arg1: T, arg2: String): Double
        [use case] Starting line
        [use case] Starting line
        The base comment. And another sentence...
        The base comment. And another sentence...
        Ending line
        Author: StartAuthor a Scala developer EndAuthor
          T       StartT the type of the first argument EndT
          arg1    Start1 The T term comment End1
          arg2    Start2 The string comment End2
          returns StartRet The return comment EndRet""", true),
    (Some("UseCaseInheritDoc"),
     """Example:   StartExample function[Int](3,"something") EndExample
        Version    StartVer 0.0.2 EndVer
        Since      StartSince 0.0.1 EndSince
        Exceptions thrown
                   SomeException      StartEx if the function is not called with correct parameters EndEx
                   SomeOtherException StartSOE Should Warn <invalid inheritdoc annotation> EndSOE
        To do      StartTodo Call mom. And dad! EndTodo
        Note       StartNote Be careful! EndNote
        See also   StartSee The Manual EndSee
     """, true))

  @Test def CommentInheritance_CorrectExplicitInheritanceForCornerCases(): Unit =
    checkText("inheritdoc-corner-cases.scala")(
      (Some("D"),
        """def hello1: Int
          Inherited: Hello 1 comment
          Inherited: Hello 1 comment
          Definition Classes D → A
       """, true),
      (Some("D"),
        """def hello2: Int
          Inherited: Hello 2 comment
          Inherited: Hello 2 comment
          Definition Classes D → B
       """, true),
      (Some("G"),
        """def hello1: Int
          Inherited: Hello 1 comment
          Inherited: Hello 1 comment
          Definition Classes G → D → A
       """, true),
      (Some("G"),
        """def hello2: Int
          Inherited: Hello 2 comment
          Inherited: Hello 2 comment
          Definition Classes G → D → B
       """, true),
      (Some("I"),
        """def hello1(i: Int): Unit
          [use case] Inherited: Hello 1 comment
          [use case] Inherited: Hello 1 comment
          Definition Classes I → G → D → A
       """, true)
      // traits E, F and H shouldn't crash scaladoc but we don't need to check the output
    )

  @Test def IndentationNormalizationForCodeBlocks(): Unit = {
    val files = createTemplates("code-indent.scala")

    files("C.html") match {
      case node: scala.xml.Node => {
        val s = node.toString
        assertTrue(s.contains("<pre>a typicial indented\ncomment on multiple\ncomment lines</pre>"))
        assertTrue(s.contains("<pre>one liner</pre>"))
        assertTrue(s.contains("<pre>two lines, one useful</pre>"))
        assertTrue(s.contains("<pre>line1\nline2\nline3\nline4</pre>"))
        assertTrue(s.contains("<pre>a ragged example\na (condition)\n  the t h e n branch\nan alternative\n  the e l s e branch</pre>"))
        assertTrue(s.contains("<pre>Trait example {\n  Val x = a\n  Val y = b\n}</pre>"))
        assertTrue(s.contains("<pre>l1\n\nl2\n\nl3\n\nl4\n\nl5</pre>"))
      }
      case _ => ???
    }
  }

  @Test def t4014_ScaladocOmitsAauthor_noAuthors(): Unit = {
    val twoAuthors = createTemplate("t4014_0.scala", "Foo.html")
    val s = twoAuthors.toString
    assertFalse(s.contains("Author"))
  }

  @Test def t4014_ScaladocOmitsAauthor_oneAuthors(): Unit = {
    val oneAuthor = createTemplate("t4014_1.scala", "Foo.html")

    val s = oneAuthor.toString
    assertTrue(s.contains("<h6>Author:</h6>"))
    assertTrue(s.contains("<p>The Only Author</p>"))
  }

  @Test def t4014_ScaladocOmitsAauthor_twoAuthors(): Unit = {
    val twoAuthors = createTemplate("t4014_2.scala", "Foo.html")

    val s = twoAuthors.toString
    assertTrue(s.contains("<h6>Authors:</h6>"))
    assertTrue(s.contains("<p>The First Author</p>"))
    assertTrue(s.contains("<p>The Second Author</p>"))
  }

  @Test def t9599_MultipleTodoFormattedWithCommaOnSeparateLine(): Unit = {
    val node = createTemplate("t9599.scala", "X.html")
    assertTrue(node.text.contains("todo3todo2todo1"))
  }
}

class HtmlFactoryTest8514 extends BaseHtmlFactoryTest {
  val files = createTemplates("basic.scala")
  //println(files)

  @Test def classDoc(): Unit = files.get("com/example/p1/Clazz.html") match {
    case Some(node: scala.xml.Node) => {
      assertTrue(node.toString contains "<span class=\"modifier\">implicit </span>")
      assertTrue(node.toString contains "title=\"gt4s: $colon$colon\"")
      assertTrue(node.toString contains "title=\"gt4s: $colon$colon$colon$colon. Deprecated: ")
    }
    case _ => ???
  }
  @Test def packageDoc(): Unit = assertTrue(files.get("com/example/p1/index.html").isDefined)

  @Test def packageObjectDoc(): Unit = files("com/example/p1/index.html") match {
    case node: scala.xml.Node =>
      assertTrue(node.toString contains "com.example.p1#packageObjectMethod")
    case _ => ???
  }

  @Test def lowerBound(): Unit = files("com/example/p1/LowerBound.html") match {
    case node: scala.xml.Node =>
    case _ => ???
  }

  @Test def upperBound(): Unit = files("com/example/p1/UpperBound.html") match {
    case node: scala.xml.Node =>
    case _ => ???
  }

  @Test def t8514_NoInconsistencies(): Unit =
    checkText("t8514.scala")(
      (Some("a/index"),
        """class A extends AnyRef
          Some doc here
          Some doc here
          Annotations @DeveloperApi()
       """, true),
      (Some("a/index"),
        """class B extends AnyRef
          Annotations @DeveloperApi()
       """, true)
    )
}

class HtmlFactoryTest8144 extends BaseHtmlFactoryTest {
  implicit class AttributesAwareNode(val node: NodeSeq) {

    def \@(attrName: String): String =
      node \ ("@" + attrName) text

    def \@(attrName: String, attrValue: String): NodeSeq =
      node filter { _ \ ("@" + attrName) exists (_.text == attrValue) }
  }

  implicit class AssertionAwareNode(node: scala.xml.NodeSeq) {

    def assertTypeLink(expectedUrl: String): Unit = {
      val linkElement: NodeSeq = node \\ "div" \@ ("id", "definition") \\ "span" \@ ("class", "permalink") \ "a"
      assertEquals(expectedUrl, linkElement \@ "href")
    }

    def assertMemberLink(group: String)(memberName: String, expectedUrl: String): Unit = {
      val linkElement: NodeSeq = node \\ "div" \@ ("id", group) \\ "li" \@ ("name", memberName) \\ "span" \@ ("class", "permalink") \ "a"
      assertEquals(expectedUrl, linkElement \@ "href")
    }

    def assertValuesLink(memberName: String, expectedUrl: String): Unit = {
      val linkElement: NodeSeq = node \\ "div" \@ ("class", "values members") \\ "li" \@ ("name", memberName) \\ "span" \@ ("class", "permalink") \ "a"
      assertEquals(expectedUrl, linkElement \@ "href")
    }

  }

  val files = createTemplates("t8144.scala")

  def check(pagePath: String): Node =
    files(pagePath) match {
      case node: xml.Node => XMLUtil.stripGroup(node)
      case _ => sys.error(s"Can't find $pagePath")
    }

  @Test def t8144_MembersPermalinkInnerPackage(): Unit = {
    val node = check("some/pack/index.html")
    node.assertTypeLink("../../some/pack/index.html")
    node.assertValuesLink("some.pack.SomeType", "../../some/pack/index.html#SomeType")
    node.assertMemberLink("types")("some.pack.SomeType", "../../some/pack/index.html#SomeTypeextendsAnyRef")
  }

  @Test def t8144_MembersPermalinkCompanionObject(): Unit = {
    val node = check("some/pack/SomeType$.html")
    node.assertTypeLink("../../some/pack/SomeType$.html")
    node.assertMemberLink("allMembers")("some.pack.SomeType#someVal", "../../some/pack/SomeType$.html#someVal:String")
  }

  @Test def t8144_MembersPermalinkClass(): Unit = {
    val node = check("some/pack/SomeType.html")
    node.assertTypeLink("../../some/pack/SomeType.html")
    node.assertMemberLink("constructors")("some.pack.SomeType#<init>", "../../some/pack/SomeType.html#<init>(arg:String):some.pack.SomeType")
    node.assertMemberLink("types")("some.pack.SomeType.TypeAlias", "../../some/pack/SomeType.html#TypeAlias=String")
    node.assertValuesLink("some.pack.SomeType#>#<", "../../some/pack/SomeType.html#>#<():Int")
    node.assertValuesLink("some.pack.SomeType#>@<", "../../some/pack/SomeType.html#>@<():SomeType.this.TypeAlias")
  }
}
