import scala.tools.partest.ReplTest

object Test extends ReplTest {

  override def code = """
def foo[S] = { type X = List[S]; List():X }
val li = foo[Int]
li: List[Int]
type TL = List[Int]
def foo = null: TL
def foo = {type TL=List[Int]; null: TL}
foo: List[Int]
  """.trim
}