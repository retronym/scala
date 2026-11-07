package lnl
object W3 { @annotation.tailrec def no(n: Int): Int = if (n == 0) 0 else 1 + no(n - 1); def miss(x: Option[Int]) = x match { case Some(_) => 3 } }
