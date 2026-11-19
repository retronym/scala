package sl
sealed abstract class S
final case class K() extends S
object H0 { val v = { class L0 extends S; new L0 } }
object H1 { val v = { class L1 extends S; new L1 } }
object H2 { val v = { class L2 extends S; new L2 } }
object H3 { val v = { class L3 extends S; new L3 } }
object H4 { val v = { class L4 extends S; new L4 } }
object H5 { val v = { class L5 extends S; new L5 } }
object H6 { val v = { class L6 extends S; new L6 } }
object H7 { val v = { class L7 extends S; new L7 } }
object H8 { val v = { class L8 extends S; new L8 } }
object H9 { val v = { class L9 extends S; new L9 } }
object H10 { val v = { class L10 extends S; new L10 } }
object H11 { val v = { class L11 extends S; new L11 } }
object H12 { val v = { class L12 extends S; new L12 } }
object H13 { val v = { class L13 extends S; new L13 } }
object H14 { val v = { class L14 extends S; new L14 } }
object H15 { val v = { class L15 extends S; new L15 } }
object Chk { def f(s: S) = s match { case K() => 1 } }
