import java.util.Locale

// The f interpolator formats with the default locale at run time, as String.format does,
// also when every argument is a literal.
object Test extends App {
  def show(s: String) = println(s.replace("İ", "<dotted I>"))
  Locale.setDefault(Locale.forLanguageTag("tr-TR"))
  val i = "i"
  show("%S".format("i"))
  show(f"$i%S")
  show(f"${"i"}%S")
  show(f"${"i"}%3S|")
}
