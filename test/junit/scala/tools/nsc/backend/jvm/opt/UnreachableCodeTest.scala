package scala.tools.nsc
package backend.jvm
package opt

import org.junit.Assert.{ assertThrows => _, _ }
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.jdk.CollectionConverters._
import scala.tools.asm.Opcodes._
import scala.tools.asm.tree.{ClassNode, LabelNode}
import scala.tools.asm.tree.analysis.{Analyzer, BasicInterpreter, BasicValue}
import scala.tools.nsc.backend.jvm.analysis.BackendUtils
import scala.tools.testkit.ASMConverters._
import scala.tools.testkit.AssertUtil._
import scala.tools.testkit.BytecodeTesting._
import scala.tools.testkit.{ClearAfterClass, Compiler}

@RunWith(classOf[JUnit4])
class UnreachableCodeTest extends ClearAfterClass {
  // jvm-1.6 enables emitting stack map frames, which impacts the code generation wrt dead basic blocks,
  // see comment in BCodeBodyBuilder
  val methodOptCompiler     = cached("methodOptCompiler", () => newCompiler(extraArgs = "-opt:l:method"))
  val dceCompiler           = cached("dceCompiler",       () => newCompiler(extraArgs = "-opt:unreachable-code"))
  val noOptCompiler         = cached("noOptCompiler",     () => newCompiler(extraArgs = "-opt:l:none"))

  def assertEliminateDead(code: (Instruction, Boolean)*): Unit = {
    val method = genMethod()(code.map(_._1): _*)
    dceCompiler.global.genBCode.postProcessor.localOpt.removeUnreachableCodeImpl(method, "C")
    val nonEliminated = instructionsFromMethod(method)
    val expectedLive = code.filter(_._2).map(_._1).toList
    assertSameCode(nonEliminated, expectedLive)
  }

  @Test
  def basicElimination(): Unit = {
    assertEliminateDead(
      Op(ACONST_NULL),
      Op(ATHROW),
      Op(RETURN).dead
    )

    assertEliminateDead(
      Op(RETURN)
    )

    assertEliminateDead(
      Op(RETURN),
      Op(ACONST_NULL).dead,
      Op(ATHROW).dead
    )
  }

  @Test
  def eliminateNop(): Unit = {
    assertEliminateDead(
      // reachable, but removed anyway.
      Op(NOP).dead,
      Op(RETURN),
      Op(NOP).dead
    )
  }

  @Test
  def eliminateBranchOver(): Unit = {
    assertEliminateDead(
      Jump(GOTO, Label(1)),
      Op(ACONST_NULL).dead,
      Op(ATHROW).dead,
      Label(1),
      Op(RETURN)
    )

    assertEliminateDead(
      Jump(GOTO, Label(1)),
      Label(1),
      Op(RETURN)
    )
  }

  @Test
  def deadLabelsRemain(): Unit = {
    assertEliminateDead(
      Op(RETURN),
      Jump(GOTO, Label(1)).dead,
      // not dead - labels may be referenced from other places in a classfile (eg exceptions table).
      // will need a different opt to get rid of them
      Label(1)
    )
  }

  @Test
  def pushPopNotEliminated(): Unit = {
    assertEliminateDead(
      // not dead, visited by data flow analysis.
      Op(ACONST_NULL),
      Op(POP),
      Op(RETURN)
    )
  }

  @Test
  def nullnessNotConsidered(): Unit = {
    assertEliminateDead(
      Op(ACONST_NULL),
      Jump(IFNULL, Label(1)),
      Op(RETURN), // not dead
      Label(1),
      Op(RETURN)
    )
  }

  @Test
  def basicEliminationCompiler(): Unit = {
    val code = "def f: Int = { return 1; 2 }"
    val withDce = dceCompiler.compileInstructions(code)
    assertSameCode(withDce.dropNonOp, List(Op(ICONST_1), Op(IRETURN)))

    val noDce = noOptCompiler.compileInstructions(code)

    // The emitted code is ICONST_1, IRETURN, ICONST_2, IRETURN. The latter two are dead.
    //
    // GenBCode puts the last IRETURN into a new basic block: it emits a label before the second
    // IRETURN. This is an implementation detail, it may change; it affects the outcome of this test.
    //
    // During classfile writing with COMPUTE_FRAMES (-target:jvm-1.6 or larger), the ClassfileWriter
    // puts the ICONST_2 into a new basic block, because the preceding operation (IRETURN) ends
    // the current block. We get something like
    //
    //   L1: ICONST_1; IRETURN
    //   L2: ICONST_2            << dead
    //   L3: IRETURN             << dead
    //
    // Finally, instructions in the dead basic blocks are replaced by NOP's and ATHROW's,
    // as explained in a comment in BCodeBodyBuilder.
    assertSameCode(noDce.dropNonOp, List(Op(ICONST_1), Op(IRETURN), Op(NOP), Op(ATHROW)))
  }

  @Test
  def eliminateDeadCatchBlocks(): Unit = {
    // the Label(1) is live: it's used in the local variable descriptor table (local variable "this" has a range from 0 to 1).
    def wrapInDefault(code: Instruction*) = List(Label(0), LineNumber(1, Label(0))) ::: code.toList ::: List(Label(1))

    val code = "def f: Int = { return 0; try { 1 } catch { case _: Exception => 2 } }"
    val m = dceCompiler.compileMethod(code)
    assertTrue(m.handlers.isEmpty) // redundant (if code is gone, handler is gone), but done once here for extra safety
    assertSameCode(m.instructions,
      wrapInDefault(Op(ICONST_0), Op(IRETURN)))

    val code2 = "def f: Unit = { try { } catch { case _: Exception => () }; () }"
    // requires fixpoint optimization of methodOptCompiler (dce alone is not enough): first the handler is eliminated, then it's dead catch block.
    assertSameCode(methodOptCompiler.compileInstructions(code2), wrapInDefault(Op(RETURN)))

    val code3 = "def f: Unit = { try { } catch { case _: Exception => try { } catch { case _: Exception => () } }; () }"
    assertSameCode(methodOptCompiler.compileInstructions(code3), wrapInDefault(Op(RETURN)))

    // this example requires two iterations to get rid of the outer handler.
    // the first iteration of DCE cannot remove the inner handler. then the inner (empty) handler is removed.
    // then the second iteration of DCE removes the inner catch block, and then the outer handler is removed.
    val code4 = "def f: Unit = { try { try { } catch { case _: Exception => () } } catch { case _: Exception => () }; () }"
    assertSameCode(methodOptCompiler.compileInstructions(code4), wrapInDefault(Op(RETURN)))
  }

  @Test // test the dce-testing tools
  def metaTest(): Unit = {
    assertThrows[AssertionError](
      assertEliminateDead(Op(RETURN).dead),
      _.contains("Expected: List()\nActual  : List(Op(RETURN))")
    )

    assertThrows[AssertionError](
      assertEliminateDead(Op(RETURN), Op(RETURN)),
      _.contains("Expected: List(Op(RETURN), Op(RETURN))\nActual  : List(Op(RETURN))")
    )
  }

  @Test
  def bytecodeEquivalence(): Unit = {
    assertTrue(List(VarOp(ILOAD, 1)) ===
               List(VarOp(ILOAD, 2)))
    assertTrue(List(VarOp(ILOAD, 1), VarOp(ISTORE, 1)) ===
               List(VarOp(ILOAD, 2), VarOp(ISTORE, 2)))

    // the first Op will associate 1->2, then the 2->2 will fail
    assertFalse(List(VarOp(ILOAD, 1), VarOp(ISTORE, 2)) ===
                List(VarOp(ILOAD, 2), VarOp(ISTORE, 2)))

    // will associate 1->2 and 2->1, which is OK
    assertTrue(List(VarOp(ILOAD, 1), VarOp(ISTORE, 2)) ===
               List(VarOp(ILOAD, 2), VarOp(ISTORE, 1)))

    assertTrue(List(Label(1), Label(2), Label(1)) ===
               List(Label(2), Label(4), Label(2)))
    assertTrue(List(LineNumber(1, Label(1)), Label(1)) ===
               List(LineNumber(1, Label(3)), Label(3)))
    assertFalse(List(LineNumber(1, Label(1)), Label(1)) ===
                List(LineNumber(1, Label(3)), Label(1)))

    assertTrue(List(TableSwitch(TABLESWITCH, 1, 3, Label(4), List(Label(5), Label(6))), Label(4), Label(5), Label(6)) ===
               List(TableSwitch(TABLESWITCH, 1, 3, Label(9), List(Label(3), Label(4))), Label(9), Label(3), Label(4)))

    assertTrue(List(FrameEntry(F_FULL, List(INTEGER, DOUBLE, Label(3)), List("java/lang/Object", Label(4))), Label(3), Label(4)) ===
               List(FrameEntry(F_FULL, List(INTEGER, DOUBLE, Label(1)), List("java/lang/Object", Label(3))), Label(1), Label(3)))
  }

  @Test
  def loadNullNothingBytecode(): Unit = {
    val code =
      """class C {
        |  def nl: Null = null
        |  def nt: Nothing = throw new Error("")
        |  def cons(a: Any) = ()
        |
        |  def t1 = cons(null)
        |  def t2 = cons(nl)
        |  def t3 = cons(throw new Error(""))
        |  def t4 = cons(nt)
        |}
      """.stripMargin
    val c = noOptCompiler.compileClass(code)

    assertSameSummary(getMethod(c, "nl"), List(ACONST_NULL, ARETURN))

    assertSameSummary(getMethod(c, "nt"), List(
      NEW, DUP, LDC, "<init>", ATHROW))

    assertSameSummary(getMethod(c, "t1"), List(
      ALOAD, ACONST_NULL, "cons", RETURN))

    // GenBCode introduces POP; ACONST_NULL after loading an expression of type scala.runtime.Null$,
    // see comment in BCodeBodyBuilder.adapt
    assertSameSummary(getMethod(c, "t2"), List(
      ALOAD, ALOAD, "nl", POP, ACONST_NULL, "cons", RETURN))

    // the bytecode generated by GenBCode is ... ATHROW; INVOKEVIRTUAL C.cons; RETURN
    // the ASM classfile writer creates a new basic block (creates a label) right after the ATHROW
    // and replaces all instructions by NOP*; ATHROW, see comment in BCodeBodyBuilder.adapt
    // NOTE: DCE is enabled by default and gets rid of the redundant code (tested below)
    assertSameSummary(getMethod(c, "t3"), List(
      ALOAD, NEW, DUP, LDC, "<init>", ATHROW, NOP, NOP, NOP, ATHROW))

    // GenBCode introduces an ATHROW after the invocation of C.nt, see BCodeBodyBuilder.adapt
    // NOTE: DCE is enabled by default and gets rid of the redundant code (tested below)
    assertSameSummary(getMethod(c, "t4"), List(
      ALOAD, ALOAD, "nt", ATHROW, NOP, NOP, NOP, ATHROW))

    val cDCE = dceCompiler.compileClass(code)
    assertSameSummary(getMethod(cDCE, "t3"), List(ALOAD, NEW, DUP, LDC, "<init>", ATHROW))
    assertSameSummary(getMethod(cDCE, "t4"), List(ALOAD, ALOAD, "nt", ATHROW))
  }

  @Test
  def patmatDefaultLineNumber(): Unit = {
    val code =
      """class Test {
        |  def test = (this: AnyRef @unchecked) match {
        |    case _: String =>
        |      "line4" // the synthetic `throw new MatchError` used to be positioned, here, despite the fact that patmat positions it at line 3.
        |  }
        |}
        |""".stripMargin
    val test: ClassNode = dceCompiler.compileClass(code)
    val i = getAsmMethod(test, "test")
    val instr = findInstrs(i, "NEW scala/MatchError").head
    val lineNumber = BytecodeUtils.previousLineNumber(instr)
    assertEquals(Some(2), lineNumber)
  }

  @Test
  def noCodeEmittedAtUnreachedJoinPoints(): Unit = {
    // There is no DCE under -opt:none: anything dead would show up here as a NOP or an extra ATHROW
    def ops(code: String) = noOptCompiler.compileInstructions(code).dropNonOp

    // both branches of the `if` in the case body jump away, so nothing reaches the end of the `if`
    val matched = ops("def f(t: (Int, Int)): Int = t match { case (a, b) => a + b }")
    assert(!matched.contains(Op(NOP)), matched)
    assertEquals(matched.toString, 1, matched.count(_ == Op(ATHROW)))

    // no return after the ATHROW of a Nothing-typed call
    val nothing = ops("def f: Int = ???")
    assertEquals(nothing.toString, Op(ATHROW), nothing.last)
    assertEquals(nothing.toString, 1, nothing.count(_ == Op(ATHROW)))
    assert(!nothing.contains(Op(IRETURN)), nothing)
  }

  @Test
  def keepLoopEnteredByJump(): Unit = {
    // the loop body is only reached by a later jump
    assertEliminateDead(
      Jump(GOTO, Label(2)),
      Label(1),
      Op(ICONST_0),
      Op(POP),
      Label(2),
      Op(ICONST_0),
      Jump(IFNE, Label(1)),
      Op(RETURN)
    )
  }

  @Test
  def deadCodeSkippedByForwardJump(): Unit = {
    assertEliminateDead(
      Jump(GOTO, Label(1)),
      Label(2),
      Op(ICONST_0).dead,
      Op(POP).dead,
      Label(1),
      Op(RETURN)
    )
  }

  @Test
  def labelStatusWithoutDeadCode(): Unit = {
    val method = genMethod()(Jump(GOTO, Label(1)), Label(2), Label(1), Op(RETURN))
    assert(!dceCompiler.global.genBCode.postProcessor.localOpt.removeUnreachableCodeImpl(method, "C"))
    val labels = method.instructions.iterator.asScala.collect { case l: LabelNode => BackendUtils.isLabelReachable(l) }.toList
    assertEquals(List(false, true), labels)
  }

  /** Instructions that no path reaches in a classfile, which is where any dead instruction handed to
   *  ASM ends up, as `nop; athrow`. Uses ASM's analyzer, so as not to depend on the code under test.
   */
  private def deadInstructions(classes: List[ClassNode]): List[String] =
    for {
      c <- classes
      m <- c.methods.asScala.toList
      if m.instructions.size > 0
      frames = new Analyzer[BasicValue](new BasicInterpreter).analyze(c.name, m)
      (insn, frame) <- m.instructions.iterator.asScala.toList.zip(frames)
      if insn.getOpcode >= 0 && frame == null
    } yield s"${c.name}.${m.name}"

  /** Links every class, which verifies it. */
  private def linked(classes: List[(String, Array[Byte])]): ClassLoader = {
    val bytes = classes.map { case (n, b) => n.stripSuffix(".class").replace('/', '.') -> b }.toMap
    val loader = new ClassLoader(getClass.getClassLoader) {
      override def findClass(n: String): Class[_] = bytes.get(n) match {
        case Some(b) => defineClass(n, b, 0, b.length)
        case _ => throw new ClassNotFoundException(n)
      }
    }
    bytes.keys.foreach(n => Class.forName(n, false, loader).getDeclaredMethods)
    loader
  }

  private def callF(loader: ClassLoader, args: AnyRef*): AnyRef = {
    val c = loader.loadClass("C")
    c.getMethods.find(_.getName == "f").get.invoke(c.getDeclaredConstructor().newInstance(), args: _*)
  }

  private val shapes =
    """class C {
      |  def g(): Unit = ()
      |  def afterReturn: Int = { return 0; try { 1 } catch { case _: Exception => 2 } }
      |  def ifBoth(x: Int): Int = { val r: Int = if (x > 0) return 1 else return 2; r }
      |  def inFinally(x: Int): Int = try { g(); x } finally { x match { case 1 => g(); case _ => return 3 } }
      |  def matchFinally(t: (Int, Int)): Int = try { t match { case (a, _) if a > 0 => return a; case (_, b) => return b } } finally g()
      |  def nothingArg(x: Int): Int = { def fail(): Nothing = throw new Exception; Integer.max(x, fail()) }
      |  def sync(x: Int): Int = synchronized { if (x > 0) return 1 else throw new Exception }
      |  def whileTrue(x: Int): Int = { var i = 0; while (true) { if (i > x) return i; i += 1 }; 7 }
      |  def tailLoop(x: Int): Int = { @annotation.tailrec def l(i: Int): Int = if (i > x) i else l(i + 1); l(0) }
      |  def lazyNothing(x: Int): String = { lazy val s: String = throw new Exception; if (x > 0) s else "n" }
      |  def switchRet(x: Int): Int = { (x: @annotation.switch) match { case 1 => return 10; case 2 => throw new Exception; case _ => return 30 } }
      |  def closure(x: Int): () => Int = () => { if (x > 0) return () => 1 else throw new Exception }
      |}
      |""".stripMargin

  @Test
  def noDeadInstructionsReachAsm(): Unit =
    for (flags <- List("", "-g:none", "-g:source", "-g:line", "-g:vars", "-g:notailcalls", "-Xcheckinit", "-Ydelambdafy:inline",
                       "-Xverify", "-opt:unreachable-code", "-opt:box-unbox", "-opt:copy-propagation", "-opt:local", "-opt:inline:**")) {
      val classes = newCompiler(extraArgs = flags).compileToBytes(shapes)
      assertEquals(flags, Nil, deadInstructions(readAsmClasses(classes)))
      linked(classes)
    }

  /** The options that leave out the pass. They enable neither `unreachable-code` nor an optimization
   *  that implies it. ASM patches what it is given, and the result has to verify.
   */
  @Test
  def deadInstructionsReachAsmWithoutThePass(): Unit =
    for (flags <- List("-opt:none", "-opt:simplify-jumps", "-opt:compact-locals", "-opt:nullness-tracking", "-opt:closure-invocations", "-opt:allow-skip-class-loading")) {
      val classes = newCompiler(extraArgs = flags).compileToBytes(shapes)
      assertTrue(flags, deadInstructions(readAsmClasses(classes)).contains("C.afterReturn"))
      linked(classes)
    }

  /** Each nested handler that becomes empty takes a further round of `methodOptimizations`, which has
   *  a limit of 10. So dead instructions do reach ASM on a default compile, if rarely, independently
   *  of what the single pass in `nothingToRemove` can prove: this method has handlers.
   */
  @Test
  def nestedEmptyHandlersExhaustRounds(): Unit = {
    def nested(depth: Int) = {
      val body = (0 until depth).foldLeft("()")((b, i) => s"try { $b } catch { case _: Exception => println($i) }")
      s"class C { def f(): String = { $body; \"ok\" } }"
    }
    val compiler = newCompiler()
    assertEquals(Nil, deadInstructions(readAsmClasses(compiler.compileToBytes(nested(9)))))
    val classes = compiler.compileToBytes(nested(10))
    assertTrue(deadInstructions(readAsmClasses(classes)).contains("C.f"))
    assertEquals("ok", callF(linked(classes)))
  }

  /** The inliner sees what the classfile has, here the output of `-opt:none`. */
  @Test
  def inlineFromClassfileWithDeadCode(): Unit = {
    val a = "class A { @inline final def m(x: Int): Int = { val r: Int = if (x > 0) return 1 else return 2; r } }"
    val dir = java.nio.file.Files.createTempDirectory("inlineFromClassfileWithDeadCode")
    try {
      val classesA = newCompiler(extraArgs = "-opt:none").compileToBytes(a)
      assertTrue(deadInstructions(readAsmClasses(classesA)).contains("A.m"))
      for ((n, b) <- classesA) java.nio.file.Files.write(dir.resolve(n), b)
      val compiler = newCompiler(extraArgs = s"-cp $dir -opt:inline:**")
      val List(c) = compiler.compileClasses("class C { def f(a: A) = a.m(1) }"): @unchecked
      assertEquals(Nil, deadInstructions(c :: Nil))
      assertDoesNotInvoke(getMethod(c, "f"), "m")
    } finally {
      java.nio.file.Files.list(dir).forEach(java.nio.file.Files.delete(_))
      java.nio.file.Files.delete(dir)
    }
  }

  /** What a compiler plugin can do to the trees before the backend: replaces calls of `jumpEnd` and
   *  `jumpLoop` by jumps to the method's `matchEnd` and loop labels.
   */
  private def jumpToLabels(compiler: Compiler)(tree: compiler.global.Tree): compiler.global.Tree = {
    import compiler.global._
    new Transformer {
      override def transform(t: Tree): Tree = t match {
        case dd: DefDef =>
          val labels = dd.rhs.collect { case l: LabelDef => l.symbol }
          def jump(prefix: String, args: List[Tree]) =
            Apply(Ident(labels.find(_.name.startsWith(prefix)).get), args).setType(definitions.NothingTpe)
          val rhs = new Transformer {
            override def transform(t: Tree): Tree = t match {
              case Apply(f, args) if f.symbol.name.string_==("jumpEnd") => jump("matchEnd", args)
              case Apply(f, _) if f.symbol.name.string_==("jumpLoop") => jump("while", Nil)
              case _ => super.transform(t)
            }
          }.transform(dd.rhs)
          treeCopy.DefDef(dd, dd.mods, dd.name, dd.tparams, dd.vparamss, dd.tpt, rhs)
        case _ => super.transform(t)
      }
    }.transform(tree)
  }

  private val jumpStubs = "def jumpEnd(x: Int): Int = ???; def jumpLoop(): Int = ???"

  @Test
  def pluginJumpsToMatchEndWithinTheMatch(): Unit = {
    val compiler = newCompiler()
    val classes = compiler.compileToBytesTransformed(
      s"""class C { $jumpStubs
         |  def f(s: Any): Int = s match { case _: String => jumpEnd(42); case _ => 1 }
         |}""".stripMargin, beforeBackend = t => jumpToLabels(compiler)(t))
    assertEquals(42, callF(linked(classes), "s"))
  }

  /** `isAtUnreachablePoint` relies on labels at the end of a match only being jumped to from earlier
   *  in the method, which holds for what the pattern matcher generates. A plugin can break that with a
   *  jump to `matchEnd` from after the match. Nothing has jumped to the end of the `if` when it is
   *  emitted, so the `POP` that the jump needs is left out. The stack heights at the label following it
   *  then differ, which fails the compile.
   */
  @Test
  def pluginJumpsToMatchEndAfterTheMatch(): Unit = {
    val compiler = newCompiler()
    val code =
      s"""class C { $jumpStubs
         |  def f(c: Boolean): Any = {
         |    var once = true
         |    if (c) ((c: Any) match { case _: String => return "s"; case _ => throw new Exception("e") }): Int else jumpLoop()
         |    while (once) { once = false; jumpEnd(42) }
         |    "done"
         |  }
         |}""".stripMargin
    assertThrows[AssertionError](compiler.compileToBytesTransformed(code, beforeBackend = t => jumpToLabels(compiler)(t)), _.contains("Error while emitting C"))
  }
}
