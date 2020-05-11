object Test extends scala.async.run.anf.AnfTransformSpec

package scala.async.run.anf {

  import language.{postfixOps, reflectiveCalls}
  import scala.concurrent.{Await, ExecutionContext, Future}
  import scala.concurrent.duration._
  import scala.tools.partest.async.Async.{async, await}
  import scala.reflect.{ClassTag, classTag}
  import org.junit.Test

  import scala.tools.partest.AsyncTest

  import Future.{successful => fut}

  import ExecutionContext.Implicits.global

  class AnfTestClass {

    def base(x: Int): Future[Int] = Future {
      x + 2
    }

    def m(y: Int): Future[Int] = async {
      val blerg = base(y)
      await(blerg)
    }

    def m2(y: Int): Future[Int] = async {
      val f = base(y)
      val f2 = base(y + 1)
      await(f) + await(f2)
    }

    def m3(y: Int): Future[Int] = async {
      val f = base(y)
      var z = 0
      if (y > 0) {
        z = await(f) + 2
      } else {
        z = await(f) - 2
      }
      z
    }

    def m4(y: Int): Future[Int] = async {
      val f = base(y)
      val z = if (y > 0) {
        await(f) + 2
      } else {
        await(f) - 2
      }
      z + 1
    }

    def futureUnitIfElse(y: Int): Future[Unit] = async {
      val f = base(y)
      if (y > 0) {
        State.result = await(f) + 2
      } else {
        State.result = await(f) - 2
      }
    }
  }

  object State {
    @volatile var result: Int = 0
  }

  class AnfTransformSpec extends AsyncTest[AnfTransformSpec]  {

    test("simple ANF transform") {
      val o = new AnfTestClass
      val fut = o.m(10)
      val res = Await.result(fut, 2 seconds)
      res mustBe (12)
    }

    test("simple ANF transform 2") {
      val o = new AnfTestClass
      val fut = o.m2(10)
      val res = Await.result(fut, 2 seconds)
      res mustBe (25)
    }

    test("simple ANF transform 3") {
      val o = new AnfTestClass
      val fut = o.m3(10)
      val res = Await.result(fut, 2 seconds)
      res mustBe (14)
    }

    test("ANF transform of assigning the result of an if-else") {
      val o = new AnfTestClass
      val fut = o.m4(10)
      val res = Await.result(fut, 2 seconds)
      res mustBe (15)
    }

    test("Unit-typed if-else in tail position") {
      val o = new AnfTestClass
      val fut = o.futureUnitIfElse(10)
      Await.result(fut, 2 seconds)
      State.result mustBe (14)
    }

    test("inlining block does not produce duplicate definition") {
      async {
        val f = 12
        val x = await(fut(f))

        {
          type X = Int
          val x: X = 42
          identity(x)
        }
        type X = Int
        x: X
      }.block
    }

    test("inlining block in tail position does not produce duplicate definition") {
      async {
        val f = 12
        val x = await(fut(f))

        {
          val x = 42
          x
        }
      }.block mustBe (42)
    }

    test("match as expression 1") {
      import ExecutionContext.Implicits.global
      val result = async {
        val x = "" match {
          case _ => await(fut(1)) + 1
        }
        x
      }
      result.block mustBe (2)
    }

    test("match as expression 2") {
      import ExecutionContext.Implicits.global
      val result = async {
        val x = "" match {
          case "" if false => await(fut(1)) + 1
          case _           => 2 + await(fut(1))
        }
        val y = x
        "" match {
          case _ => await(fut(y)) + 100
        }
      }
      result.block mustBe (103)
    }

    test("nestedAwaitAsBareExpression") {
      import ExecutionContext.Implicits.global
      val result = async {
        await(fut(await(fut("")).isEmpty))
      }
      result.block mustBe (true)
    }

    test("nestedAwaitInBlock") {
      import ExecutionContext.Implicits.global
      val result = async {
        ()
        await(fut(await(fut("")).isEmpty))
      }
      result.block mustBe (true)
    }

    test("nestedAwaitInIf") {
      import ExecutionContext.Implicits.global
      val result = async {
        if ("".isEmpty)
          await(fut(await(fut("")).isEmpty))
        else 0
      }
      result.block mustBe (true)
    }

    test("byNameExpressionsArentLifted") {
      def foo(ignored: => Any, b: Int) = b
      val result = async {
        foo(???, await(fut(1)))
      }
      result.block mustBe (1)
    }

    test("evaluationOrderRespected") {
      def foo(a: Int, b: Int) = (a, b)
      val result = async {
        var i = 0
        def next() = {
          i += 1
          i
        }
        foo(next(), await(fut(next())))
      }
      result.block mustBe ((1, 2))
    }

    test("awaitInNonPrimaryParamSection1") {
      def foo(a0: Int)(b0: Int) = s"a0 = $a0, b0 = $b0"
      val res = async {
        var i = 0
        def get = {i += 1; i}
        foo(get)(await(fut(get)))
      }
      res.block mustBe "a0 = 1, b0 = 2"
    }

    test("awaitInNonPrimaryParamSection2") {
      def foo[T](a0: Int)(b0: Int*) = s"a0 = $a0, b0 = ${b0.head}"
      val res = async {
        var i = 0
        def get = async{i += 1; i}
        foo[Int](await(get))(await(get) :: await(async(Nil)) : _*)
      }
      res.block mustBe "a0 = 1, b0 = 2"
    }

    test("awaitInNonPrimaryParamSectionWithLazy1") {
      def foo[T](a: => Int)(b: Int) = b
      val res = async {
        def get = async {0}
        foo[Int](???)(await(get))
      }
      res.block mustBe 0
    }

    test("awaitInNonPrimaryParamSectionWithLazy2") {
      def foo[T](a: Int)(b: => Int) = a
      val res = async {
        def get = async {0}
        foo[Int](await(get))(???)
      }
      res.block mustBe 0
    }

    test("awaitWithLazy") {
      def foo[T](a: Int, b: => Int) = a
      val res = async {
        def get = async {0}
        foo[Int](await(get), ???)
      }
      res.block mustBe 0
    }

    test("awaitOkInReciever") {
      class Foo { def bar(a: Int)(b: Int) = a + b }
      async {
        await(async(new Foo)).bar(1)(2)
      }
    }

    test("namedArgumentsRespectEvaluationOrder") {
      def foo(a: Int, b: Int) = (a, b)
      val result = async {
        var i = 0
        def next() = {
          i += 1
          i
        }
        foo(b = next(), a = await(fut(next())))
      }
      result.block mustBe ((2, 1))
    }

    test("namedAndDefaultArgumentsRespectEvaluationOrder") {
      var i = 0
      def next() = {
        i += 1
        i
      }
      def foo(a: Int = next(), b: Int = next()) = (a, b)
      async {
        foo(b = await(fut(next())))
      }.block mustBe ((2, 1))
      i = 0
      async {
        foo(a = await(fut(next())))
      }.block mustBe ((1, 2))
    }

    test("repeatedParams1") {
      var i = 0
      def foo(a: Int, b: Int*) = b.toList
      def id(i: Int) = i
      async {
        foo(await(fut(0)), id(1), id(2), id(3), await(fut(4)))
      }.block mustBe (List(1, 2, 3, 4))
    }

    test("repeatedParams2") {
      var i = 0
      def foo(a: Int, b: Int*) = b.toList
      def id(i: Int) = i
      async {
        foo(await(fut(0)), List(id(1), id(2), id(3)): _*)
      }.block mustBe (List(1, 2, 3))
    }

    test("awaitInThrow") {
      intercept[Exception](
        async {
          throw new Exception("msg: " + await(fut(0)))
        }.block
      ).getMessage mustBe "msg: 0"
    }

    test("awaitInTyped") {
      async {
        (("msg: " + await(fut(0))): String).toString
      }.block mustBe "msg: 0"
    }


    test("awaitInAssign") {
      async {
        var x = 0
        x = await(fut(1))
        x
      }.block mustBe 1
    }

    test("caseBodyMustBeTypedAsUnit") {
      val Up = 1
      val Down = 2
      val sign = async {
        await(fut(1)) match {
          case Up   => 1.0
          case Down => -1.0
        }
      }
      sign.block mustBe 1.0
    }

//    @Test
//    def awaitInImplicitApply(): Unit = {
//      val tb = mkToolbox(s"-cp ${toolboxClasspath}")
//      val tree = tb.typeCheck(tb.parse {
//        """
//          | import language.implicitConversions
//          | import _root_.scala.tools.partest.async.Async.{async, await}
//          | import _root_.scala.concurrent._
//          | import ExecutionContext.Implicits.global
//          | implicit def view(a: Int): String = ""
//          | async {
//          |   await(0).length
//          | }
//        """.stripMargin
//      })
//      val applyImplicitView = tree.collect { case x if x.getClass.getName.endsWith("ApplyImplicitView") => x }
//      println(applyImplicitView)
//      applyImplicitView.map(_.toString) mustStartWith List("view(")
//    }

    test("nothingTypedIf") {
      val result = util.Try(async {
        if (true) {
          val n = await(fut(1))
          if (n < 2) {
            throw new RuntimeException("case a")
          }
          else {
            throw new RuntimeException("case b")
          }
        }
        else {
          "case c"
        }
      }.block)

      assert(result.asInstanceOf[util.Failure[_]].exception.getMessage == "case a")
    }

    test("awaitInArrayValue") {
      val result = async {
        Array(1, await(fut(2)), await(fut(3))).sum
      }.block

      result mustBe 6
    }

    test("nothingTypedMatch") {
      val result = util.Try(async {
        0 match {
          case _ if "".isEmpty =>
            val n = await(fut(1))
            n match {
              case _ if n < 2 =>
                throw new RuntimeException("case a")
              case _ =>
                throw new RuntimeException("case b")
            }
          case _ =>
            "case c"
        }
      }.block)

      assert(result.asInstanceOf[util.Failure[_]].exception.getMessage == "case a")
    }
  }

}
