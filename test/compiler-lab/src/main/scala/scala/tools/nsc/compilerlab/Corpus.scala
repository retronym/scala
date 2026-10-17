package scala.tools.nsc.compilerlab

import java.net.URI
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, Paths, StandardCopyOption}

import scala.jdk.CollectionConverters._
import scala.reflect.io.{AbstractFile, VirtualDirectory}

/** Sources compiled together, read into memory once, with what they need to compile.
 *
 *  @param files       the sources, in an in-memory directory tree
 *  @param sourcePath  the root of that tree, if the compiler should also find them as on `-sourcepath`
 *  @param deps        jars on the classpath besides the JDK and, if `withScala`, the quick library, reflect and compiler
 *  @param jdkModules  the JDK modules on the classpath; scalac on JDK 9+ sees them all unless given `-release`
 */
final case class Corpus(name: String, files: List[AbstractFile], options: List[String], deps: List[Path],
    jdkModules: List[String], withScala: Boolean = true, sourcePath: Option[AbstractFile] = None) {
  /** What the classpath depends on: corpora that agree share one snapshot. */
  def classpathKey: Corpus.Key = Corpus.Key(jdkModules, withScala, deps)
}

object Corpus {
  final case class Key(jdkModules: List[String], withScala: Boolean, deps: List[Path])

  /** As scala/compiler-benchmark compiles its corpora. */
  private val sharedOptions = List("-nowarn")

  private final case class Known(options: List[String] = Nil, withScala: Boolean = true, sourcePath: Boolean = false)

  // Those that compile with 2.13 on JDK 17+: better-files needs javax.xml.bind (JDK 8), vector is 2.12's collections,
  // scalactic-position calls a macro of a 2.12 jar, induction overflows a 2 MB stack.
  private val known = Map(
    "map-seq-inference" -> Known(),
    "patmat-exhaust-huge" -> Known(),
    "re2s" -> Known(),
    "scalap" -> Known(),
    // the library, reflect and the compiler from source, the library also as the sourcepath
    "scala" -> Known(withScala = false, sourcePath = true),
  )

  /** A corpus of a scala/compiler-benchmark checkout (`compilerlab.corpora` is its `corpus` directory), from
   *  `<name>/latest`, or, given an absolute path, the sources under that directory.
   */
  def load(name: String): Corpus = {
    val absolute = Paths.get(name).isAbsolute
    val dir =
      if (absolute) Paths.get(name)
      else Paths.get(prop("compilerlab.corpora")).resolve(name).resolve("latest")
    val k = known.get(if (absolute) dir.getFileName.toString else name) match {
      case Some(k) => k
      case None if absolute => Known()
      case None => sys.error(s"unknown corpus $name; known: ${known.keys.toList.sorted.mkString(", ")}, or an absolute path")
    }
    val root = new VirtualDirectory(s"memory-$name", None)
    val paths = Files.walk(dir, java.nio.file.FileVisitOption.FOLLOW_LINKS).iterator.asScala.filter(p => isSource(p.toString)).toList.sorted
    if (paths.isEmpty) sys.error(s"no sources in $dir")
    val files = paths.map { p =>
      val rel = dir.relativize(p).toString
      val parts = rel.split('/').toList
      val file = parts.init.foldLeft(root: AbstractFile)(_.subdirectoryNamed(_)).fileNamed(parts.last)
      val out = file.output
      try out.write(filter(new String(Files.readAllBytes(p), UTF_8)).getBytes(UTF_8)) finally out.close()
      file
    }
    Corpus(name, files, sharedOptions ++ k.options, deps(dir), jdkModules, k.withScala,
      if (k.sourcePath) Some(root.lookupName("library", directory = true)) else None)
  }

  private def prop(key: String): String = sys.props.getOrElse(key, sys.error(s"-D$key not set"))

  private def isSource(path: String) = path.endsWith(".scala") || path.endsWith(".java")

  /** `compilerlab.jdkModules`, comma separated, or all the modules of that JDK (`InMemoryClasspath.jdkImage`), as scalac's `JrtClassPath` sees them. */
  private def jdkModules: List[String] = sys.props.get("compilerlab.jdkModules") match {
    case Some(ms) => ms.split(",").toList.map(_.trim).filter(_.nonEmpty)
    case None =>
      val modules = InMemoryClasspath.jdkImage
      val list = Files.list(modules)
      try list.iterator.asScala.map(_.getFileName.toString).toList.sorted finally list.close()
  }

  /** The jars named in the corpus's `deps.txt` (URLs), downloaded once into `compilerlab.deps`. */
  private def deps(dir: Path): List[Path] = {
    val list = dir.resolve("deps.txt")
    if (!Files.exists(list)) Nil
    else Files.readAllLines(list).asScala.toList.map(_.trim).filter(_.nonEmpty).map { url =>
      val cache = Paths.get(prop("compilerlab.deps"))
      val jar = cache.resolve(Paths.get(new URI(url).getPath).getFileName.toString)
      if (!Files.exists(jar)) {
        Files.createDirectories(cache)
        val tmp = Files.createTempFile(cache, "download", ".jar")
        // some deps.txt entries are http:, which Maven Central now refuses
        val in = new URI(url.replaceFirst("^http:", "https:")).toURL.openStream()
        try Files.copy(in, tmp, StandardCopyOption.REPLACE_EXISTING) finally in.close()
        Files.move(tmp, jar, StandardCopyOption.ATOMIC_MOVE)
      }
      jar
    }
  }

  /** scala/compiler-benchmark's filter lines: `//#if 2.12|2.13`, `//#else`, `//#fi`, matched against this compiler's
   *  version. The filter lines themselves are dropped, as there.
   */
  private def filter(text: String): String = {
    if (!text.contains("//#")) return text
    val version = scala.util.Properties.versionNumberString
    var on = true
    var matched = false
    val sb = new java.lang.StringBuilder(text.length)
    for (line <- text.linesWithSeparators) {
      val t = line.trim
      if (t.startsWith("//#")) {
        val words = t.split(" ").filter(_.nonEmpty)
        words(0) match {
          case "//#if" =>
            val matches = words.length > 1 && words(1).split('|').exists(version.startsWith)
            on = matches && !matched
            if (on) matched = true
          case "//#else" => on = !matched
          case "//#fi" => on = true; matched = false
          case other => sys.error(s"unknown filter line $other")
        }
      } else if (on) sb.append(line)
    }
    sb.toString
  }
}
