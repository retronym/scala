object Test {
  def main(args: Array[String]): Unit = {
    B_2.run()
    classOf[A].getDeclaredMethods.filter(_.getName.startsWith("foo")).sortBy(_.getName).foreach { m =>
      // spelled out: `Method.toGenericString` omits the bounds of type parameters on JDK 8
      val tparams = m.getTypeParameters.map(tp => tp.getName + tp.getBounds.map(_.getTypeName).mkString(" <: ", " & ", ""))
      println(tparams.mkString(m.getName + "[", ", ", "]"))
    }
  }
}
