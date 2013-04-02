object Test extends App {
  import Macros._

  object PathList {
    def unapplySeq(path: String): Option[Seq[String]] = {
      val split = path.split("/")
      if (split.size == 0) None
      else Some(split.toList)
    }
  }
  def m =
    demo(Seq[String => String]({
      case PathList(ps @ _*) => ps.last
    }))
}
