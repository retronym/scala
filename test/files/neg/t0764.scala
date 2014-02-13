class Top[A] {
        type AType = A
}

trait Node { outer =>
        type T <: Node
        def prepend = new Node { type T = outer.type }
}

class Main[NextType <: Node](value: Node { type T = NextType })
        extends Top[Node { type T = NextType }] {

  val pre  : Node { type T = _1.type  } forSome { val _1: Node{type T = NextType} } = (value: AType).prepend
  def wrong: Node { type T = NextType } = pre

  new Main[AType](pre)
}

/* this used to be a neg test, even though it should've compiled
SI-8177 fixed this.

Behold the equivalent program which type checks without the fix for SI-8177.
(Expand type alias, convert type member to type param;
note the covariance to encode subtyping on type members.)

class Node[+T <: Node[_]] { def prepend = new Node[this.type] }
class Main[NextType <: Node[_]](value: Node[NextType]) {
  new Main(value.prepend)
}

UPDATE:

That's not the same program. This should be a neg. I've put in explicit
types for `pre` to show my reasoning.
*/