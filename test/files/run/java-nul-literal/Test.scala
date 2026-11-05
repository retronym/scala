//> using jvm 15+
// The constants are inlined from the values the Scala compiler parsed from Nul.java,
// compare them with the values that javac compiled into Nul.class.
object Test extends App {
  def show(name: String, parsed: Any): Unit = {
    val compiled = classOf[Nul].getField(name).get(null)
    val codes = parsed.toString.map(_.toInt).mkString(",")
    println(s"$name: $codes${if (parsed == compiled) "" else " (javac: " + compiled.toString.map(_.toInt).mkString(",") + ")"}")
  }
  show("Empty", valueOf[Nul.Empty.type])
  show("Mixed", valueOf[Nul.Mixed.type])
  show("Octal", valueOf[Nul.Octal.type])
  show("C0", valueOf[Nul.C0.type])
  show("C1", valueOf[Nul.C1.type])
  show("Block", valueOf[Nul.Block.type])
}
