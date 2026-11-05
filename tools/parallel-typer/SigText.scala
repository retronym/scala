// The pickles (.sig files) under a directory as text, one file per pickle, with scalap's printer: a diff of the text
// of two pickles shows which signatures differ, where a diff of the bytes only shows that they do.
//   SigText <sig dir> <out dir>
import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters._
import scala.tools.scalap.Main
import scala.tools.scalap.scalax.rules.scalasig.{ByteCode, ScalaSigAttributeParsers}

object SigText {
  def main(args: Array[String]): Unit = {
    val in = Paths.get(args(0)); val out = Paths.get(args(1))
    Main.printPrivates = true
    val files = Files.walk(in).iterator.asScala.filter(_.toString.endsWith(".sig")).toList
    for (f <- files) {
      val rel = in.relativize(f).toString
      val text =
        try {
          val sig = ScalaSigAttributeParsers.parse(ByteCode(Files.readAllBytes(f)))
          Main.parseScalaSignature(sig, Main.isPackageObjectFile(rel.stripSuffix(".sig").replace('/', '.')))
        } catch { case ex: Throwable => s"<unreadable: $ex>" }
      val target = out.resolve(rel + ".txt")
      Files.createDirectories(target.getParent)
      Files.write(target, text.getBytes("UTF-8"))
    }
  }
}
