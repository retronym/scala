package scala.reflect.internal

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.tools.nsc.symtab.SymbolTableForUnitTesting

@RunWith(classOf[JUnit4])
class AsSeenFromTest {

  object symbolTable extends SymbolTableForUnitTesting

  import symbolTable._

  @Test
  def asSeenFrom(): Unit = {
    import asSeenFromTypes._
    assertEquals(typeOf[Int], fooResult(typeOf[B]))
    assertEquals(typeOf[Int], fooResult(typeOf[B#B_I]))
    assertEquals(typeOf[String], fooResult(typeOf[C#B_I]))
  }

  @Test def asSeenFrom2(): Unit = {
    import asSeenFrom2Types._
    val seenFronTyoe = fooResult(ThisType(symbolOf[O2#I2]))
    assertEquals(TypeRef(ThisType(symbolOf[O2]), typeOf[O1[_]].member(TypeName("O1_A_Alias")), Nil), seenFronTyoe)
    assertTrue(typeOf[Int] =:= seenFronTyoe)
  }

  @Test def t21585(): Unit = {
    // https://youtrack.jetbrains.com/issue/SCL-21585
    import t21585Types._

    // apply(O1.this.O1_A_Alias : NullaryMethodType)
    //  apply(O1.this.O1_A_Alias : AliasNoArgsTypeRef)
    //    apply(O1.this.type : UniqueThisType)
    //      thisTypeAsSeen(O1.this.type)
    //        matchesPrefixAndClass(pre=I2.this.type, class=trait I1)(candidate=trait O1)
    //        = false
    //        matchesPrefixAndClass(pre=O2.this.type, class=trait O1)(candidate=trait O1)
    //        = true
    //      = O2.this.type
    //    = O2.this.type
    //  = O2.this.O1_A_Alias
    //= O2.this.O1_A_Alias
    val seenFromType = fooResult(ThisType(typeOf[O2].member(TypeName("I2"))))
    assertEquals(TypeRef(ThisType(symbolOf[O2]), typeOf[O1[_]].member(TypeName("O1_A_Alias")), Nil), seenFromType)
    assertTrue(typeOf[Int] =:= seenFromType)
  }

  private def fooResult(pre: Type) = {
    val member = pre.member(TermName("foo"))
    val tpe    = member.info
    class LoggingAsSeenFromMap(seenFromPrefix: Type, seenFromClass: Symbol, debug: Boolean = false, var indentLevel: Int = 0)
      extends AsSeenFromMap(seenFromPrefix, seenFromClass) {

      def logged[T](message: String, op: => T): T = if (debug) {
        println(s"${"  " * indentLevel}${message.replace('\n', ' ')}")
        indentLevel += 1
        val result = op
        indentLevel -= 1
        println(s"${"  " * indentLevel}= $result")
        result
      } else op

      override def apply(tp: Type): Type = {
        logged(s"apply($tp : ${tp.getClass.getSimpleName})", super.apply(tp))
      }

      override protected def correspondingTypeArgument(lhs: Type, rhs: Type): Type =
        logged(s"correspondingTypeArgument($lhs, $rhs)", super.correspondingTypeArgument(lhs, rhs))
      override protected def matchesPrefixAndClass(pre: Type, clazz: Symbol)(candidate: Symbol): Boolean =
        logged(s"matchesPrefixAndClass(pre=$pre, clazz=$clazz)(candidate=$candidate)", super.matchesPrefixAndClass(pre, clazz)(candidate))
      override protected def classParameterAsSeen(classParam: TypeRef): Type =
        logged(s"classParameterAsSeen($classParam)", super.classParameterAsSeen(classParam))
      override protected def thisTypeAsSeen(tp: ThisType): Type =
        logged(s"thisTypeAsSeen($tp)", super.thisTypeAsSeen(tp))
      override protected def classParameterAsSeenImpl(classParam: TypeRef, pre: Type, clazz: Symbol): Type =
        logged(s"classParameterAsSeenImpl(..., pre=$pre, clazz=$clazz)", super.classParameterAsSeenImpl(classParam, pre, clazz))
      override def thisTypeAsSeenImpl(tp: ThisType, pre: Type, clazz: Symbol): Type =
        logged(s"thisTypeAsSeenImpl(..., pre=$pre, clazz=$clazz)", super.thisTypeAsSeenImpl(tp, pre, clazz))
    }
    new LoggingAsSeenFromMap(pre, member.owner, debug = true).apply(tpe).resultType
  }
}

object asSeenFromTypes {
  trait A[X] {
    def foo: X

    trait A_I {
      def foo: X
    }
  }

  trait B extends A[Int] {
    trait B_I extends A_I
  }

  trait C extends A[Int] {
    trait B_I extends A[String]
  }
}

object asSeenFrom2Types {

  trait O1[O1_A] {
    type O1_A_Alias = O1_A

    trait I1[I1_A] {
      type I1_A_Alias = I1_A
      def foo: O1_A_Alias
      def bar(x: O1_A_Alias): Unit
    }
  }

  trait O2 extends O1[Int] {
    trait I2 extends I1[String] with O1[Nothing] {
      self: X =>
      bar(self.foo)
    }
  }

  trait X

  val o2 : O2           = ???
  val pre: o2.I2 with X = ???
}


object t21585Types {

  trait O1[O1_A] {
    type O1_A_Alias = O1_A

    trait I1 {
      def foo: O1_A_Alias
      def bar(p: O1_A_Alias): Unit = ()
    }
  }

  trait O2 extends O1[Int] {
    trait I2 extends I1 with O1[String] {
      def goodCodeRed() = {
        var x = this.foo
        x = 1 // good code red
        this.bar(1) // good code red
      }

      //      def badCodeGreen() = {
      //        var x = this.foo
      //        x = "" // bad code green
      //        this.bar("") // bad code green
      //      }
    }
  }
}
