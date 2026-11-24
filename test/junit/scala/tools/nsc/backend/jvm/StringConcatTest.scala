package scala.tools.nsc
package backend.jvm

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.jdk.CollectionConverters._
import scala.tools.asm.Type
import scala.tools.asm.tree.InvokeDynamicInsnNode
import scala.tools.testkit.ASMConverters._
import scala.tools.testkit.BytecodeTesting
import scala.tools.testkit.BytecodeTesting._
import scala.util.Properties.isJavaAtLeast

@RunWith(classOf[JUnit4])
class StringConcatTest extends BytecodeTesting {
  import compiler._

  @Test
  def appendOverloadNoBoxing(): Unit = {
    val code =
      """class C {
        |  def t1(
        |         v: Unit,
        |         z: Boolean,
        |         c: Char,
        |         b: Byte,
        |         s: Short,
        |         i: Int,
        |         l: Long,
        |         f: Float,
        |         d: Double,
        |         str: String,
        |         sbuf: java.lang.StringBuffer,
        |         chsq: java.lang.CharSequence,
        |         chrs: Array[Char]) = str + this + v + z + c + b + s + i + f + l + d + sbuf + chsq + chrs
        |
        |  def t2(
        |         v: Unit,
        |         z: Boolean,
        |         c: Char,
        |         b: Byte,
        |         s: Short,
        |         i: Int,
        |         l: Long,
        |         f: Float,
        |         d: Double,
        |         str: String,
        |         sbuf: java.lang.StringBuffer,
        |         chsq: java.lang.CharSequence,
        |         chrs: Array[Char]) = s"$str$this$v$z$c$b$s$i$f$l$d$sbuf$chsq$chrs"
        |
        |  def t3(
        |         v: Unit,
        |         z: Boolean,
        |         c: Char,
        |         b: Byte,
        |         s: Short,
        |         i: Int,
        |         l: Long,
        |         f: Float,
        |         d: Double,
        |         str: String,
        |         sbuf: java.lang.StringBuffer,
        |         chsq: java.lang.CharSequence,
        |         chrs: Array[Char]) = raw"$str$this$v$z$c$b$s$i$f$l$d$sbuf$chsq$chrs"
        |
        |}
      """.stripMargin
    val c = compileClass(code)

    def invokeNameDesc(m: String): List[String] = getInstructions(c, m) collect {
      case Invoke(_, _, name, desc, _) => name + desc
    }
    val t1Expected = List(
      "<init>(I)V",
      "append(Ljava/lang/String;)Ljava/lang/StringBuilder;",
      "append(Ljava/lang/Object;)Ljava/lang/StringBuilder;",
      "append(Ljava/lang/Object;)Ljava/lang/StringBuilder;",
      "append(Z)Ljava/lang/StringBuilder;",
      "append(C)Ljava/lang/StringBuilder;",
      "append(I)Ljava/lang/StringBuilder;",
      "append(I)Ljava/lang/StringBuilder;",
      "append(I)Ljava/lang/StringBuilder;",
      "append(F)Ljava/lang/StringBuilder;",
      "append(J)Ljava/lang/StringBuilder;",
      "append(D)Ljava/lang/StringBuilder;",
      "append(Ljava/lang/StringBuffer;)Ljava/lang/StringBuilder;",
      "append(Ljava/lang/CharSequence;)Ljava/lang/StringBuilder;",
      "append(Ljava/lang/Object;)Ljava/lang/StringBuilder;", // test that we're not using the [C overload
      "toString()Ljava/lang/String;")
    assertEquals(invokeNameDesc("t1"), t1Expected)

    // intrinsics for StringContext.{raw,s}
    assertEquals(invokeNameDesc("t2"), t1Expected)
    assertEquals(invokeNameDesc("t3"), t1Expected)
  }

  @Test
  def concatPrimitiveCorrectness(): Unit = {
    val obj: Object = new { override def toString = "TTT" }
    def t(
           v: Unit,
           z: Boolean,
           c: Char,
           b: Byte,
           s: Short,
           i: Int,
           l: Long,
           f: Float,
           d: Double,
           str: String,
           sbuf: java.lang.StringBuffer,
           chsq: java.lang.CharSequence,
           chrs: Array[Char]) = {
      str + obj + v + z + c + b + s + i + f + l + d + sbuf + chsq + chrs
    }
    def sbuf = { val r = new java.lang.StringBuffer(); r.append("sbuf"); r }
    def chsq: java.lang.CharSequence = "chsq"
    val s = t((), z=true, 'd', 3: Byte, 12: Short, 3, -32L, 12.3f, -4.2d, "me", sbuf, chsq, Array('a', 'b'))
    val r = s.replaceAll("""\[C@\w+""", "<ARRAY>")
    assertEquals(r, "meTTT()trued312312.3-32-4.2sbufchsq<ARRAY>")
  }

  @Test
  def indyConcatManySpills(): Unit = if (isJavaAtLeast(11)) {
    // 19900 operands of two slots each are spilled into 202 calls of `makeConcatWithConstants` (at most
    // 199 slots each). The partial strings cannot be concatenated by one call, which takes at most 200 slots.
    val operands = 19900
    val concat = List.fill(operands)("l").mkString("\"<\" + ", " + ", "")
    val code = s"class C { def t(l: Long): String = $concat }"
    val indyCompiler = BytecodeTesting.newCompiler(extraArgs = "-release:11")
    var classes: List[(String, Array[Byte])] = Nil
    // the deeply nested expression needs a large stack to compile
    val compileThread = new Thread(null, () => classes = indyCompiler.compileToBytes(code), "indyConcatManySpills", 1L << 30)
    compileThread.start()
    compileThread.join()
    val List((_, bytes)) = classes: @unchecked

    val indySlots = getAsmMethod(readAsmClasses(classes).head, "t").instructions.iterator.asScala.collect {
      case indy: InvokeDynamicInsnNode => (Type.getArgumentsAndReturnSizes(indy.desc) >> 2) - 1
    }.toList
    assertEquals(204, indySlots.size) // 202 spills, 2 calls to concatenate the 202 partial strings
    assertTrue(indySlots.toString, indySlots.forall(_ <= 200))

    val loader = new ClassLoader(getClass.getClassLoader) {
      override def findClass(name: String): Class[_] = defineClass(name, bytes, 0, bytes.length)
    }
    val c = loader.loadClass("C")
    val res = c.getMethod("t", classOf[Long]).invoke(c.getDeclaredConstructor().newInstance(), Long.box(7L))
    assertEquals("<" + "7" * operands, res)
  }
}
