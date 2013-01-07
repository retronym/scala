import util.control.NonFatal

object Test {
	def main(args: Array[String]) {
		Predef.println("") // load Predef
		loop
	}
  def loop: Unit = try {
  	loop
  } catch {
  	case util.control.NonFatal(e) => println("okay")
  }
}
