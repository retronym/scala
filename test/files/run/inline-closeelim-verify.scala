package p1 {
  class C {
    @inline final def log(msg: => AnyRef) {
      if ("".isEmpty) msg.toString
    }

    // This code was begin optimized into a Verify errpr after my first
    // attempt to disable copy propagation in closure-elim for monitor
    // instructions.
    //
    // The symptom, and my change that caused it, sounds the same as the one
    // mentioned in this comment: https://github.com/scala/scala/blob/1ec397340c30abf0638dd469fb4815cdd266c36b/src/compiler/scala/tools/nsc/backend/icode/BasicBlocks.scala#L199-L220
    def resolveAmbiguousImport(imp1: AnyRef, imp2: AnyRef): Unit = {
      val imp1Symbol: AnyRef = ""
      val imp2Symbol: AnyRef = ""
      def t1: AnyRef = imp1.toString
      def t2: AnyRef = imp2.toString
      def mt1: AnyRef = (t1, imp1Symbol)
      def mt2: AnyRef= (t2, imp2Symbol)

      if (t1 eq t2)
        log(t1)
      else if (mt1 eq mt2)
        log(mt1)
    }
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    new p1.C().resolveAmbiguousImport("", "")
  }
}
