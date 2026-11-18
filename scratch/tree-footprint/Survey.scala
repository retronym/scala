//> using scala 2.13.18
//> using dep org.scala-lang:scala-compiler:2.13.18
//> using dep org.openjdk.jol:jol-core:0.17
//> using javaOpt -Xmx12g -Xss16m -Djdk.attach.allowAttachSelf=true --add-opens=java.base/java.util=ALL-UNNAMED --add-opens=java.base/java.lang=ALL-UNNAMED --add-opens=java.base/java.lang.ref=ALL-UNNAMED --add-opens=java.base/java.util.concurrent=ALL-UNNAMED --add-opens=java.base/java.util.concurrent.atomic=ALL-UNNAMED --add-opens=java.base/java.io=ALL-UNNAMED

import java.lang.management.ManagementFactory
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, Paths}
import javax.management.ObjectName
import scala.collection.mutable
import scala.jdk.CollectionConverters._
import scala.reflect.internal.util.{BatchSourceFile, Position}
import scala.tools.nsc.{Global, Settings}
import scala.tools.nsc.reporters.ConsoleReporter

/** Survey Tree/Position counts retained after typer, two ways:
 *  - "walk": identity-deduped traversal of unit bodies (incl. TypeTree.original), with each tree's rawatt
 *  - "heap": live class histogram (GC.class_histogram) after typer, all trees on the heap
 *  Prints TSV lines prefixed by `W\t` / `H\t` for the analysis step.
 */
object Survey {
  val corpusRoot = Paths.get(sys.props.getOrElse("survey.corpora", sys.props("user.home") + "/code/scala/compiler-benchmark/corpus"))

  def filter(text: String): String = {
    if (!text.contains("//#")) return text
    val version = "2.13"
    var on = true; var matched = false
    val sb = new java.lang.StringBuilder(text.length)
    for (line <- text.linesWithSeparators) {
      val t = line.trim
      if (t.startsWith("//#")) {
        val words = t.split(" ").filter(_.nonEmpty)
        words(0) match {
          case "//#if" => val m = words.length > 1 && words(1).split('|').exists(version.startsWith); on = m && !matched; if (on) matched = true
          case "//#else" => on = !matched
          case "//#fi" => on = true; matched = false
        }
      } else if (on) sb.append(line)
    }
    sb.toString
  }

  def jar(c: Class[_]) = c.getProtectionDomain.getCodeSource.getLocation.getPath

  def main(args: Array[String]): Unit = {
    val name = args(0)
    val dir = corpusRoot.resolve(name).resolve("latest").toRealPath()
    val paths = Files.walk(dir).iterator.asScala.map(_.toString).filter(p => p.endsWith(".scala") || p.endsWith(".java")).toList.sorted
    val settings = new Settings(sys.error)
    val scalaLib = classOf[scala.Option[_]].getProtectionDomain.getCodeSource.getLocation.getPath
    if (name == "scala") {
      val asm = Files.list(Paths.get(sys.props.getOrElse("survey.deps", sys.props("user.home") + "/code/retronym/jvm-perf-lab/labs/scala2/target/deps"))).iterator.asScala.find(_.toString.contains("scala-asm")).map(_.toString)
      settings.classpath.value = asm.getOrElse(sys.error("scala-asm jar"))
      settings.sourcepath.value = dir.resolve("library").toString
    } else settings.classpath.value = List(scalaLib, jar(classOf[scala.reflect.internal.SymbolTable]), jar(classOf[Global])).mkString(java.io.File.pathSeparator)
    settings.processArgumentString("-Ystop-after:typer -nowarn -d " + Files.createTempDirectory("survey").toString)
    val reporter = new ConsoleReporter(settings)
    val global = new Global(settings, reporter)
    val run = new global.Run
    val sources = paths.map(p => new BatchSourceFile(p, filter(new String(Files.readAllBytes(Paths.get(p)), UTF_8))))
    val t0 = System.nanoTime
    run.compileSources(sources)
    System.err.println(s"compiled ${sources.size} files in ${(System.nanoTime - t0) / 1000000} ms, errors=${reporter.hasErrors}")

    import global._
    val trees = new java.util.IdentityHashMap[Tree, Unit]
    val atts  = new java.util.IdentityHashMap[AnyRef, Unit]
    val treeCls = mutable.Map[String, Long]().withDefaultValue(0L)
    val attCls  = mutable.Map[String, Long]().withDefaultValue(0L)
    val sharedPos = mutable.Map[String, Long]().withDefaultValue(0L) // trees whose rawatt was already seen on another tree
    val walker = new Traverser {
      override def traverse(t: Tree): Unit = if (t != null && !trees.containsKey(t)) {
        trees.put(t, ())
        treeCls(t.getClass.getName) += 1
        val a = t.attachments
        if (atts.containsKey(a)) sharedPos(a.getClass.getName) += 1
        else {
          atts.put(a, ())
          attCls(a.getClass.getName) += 1
          if (!a.isInstanceOf[Position] && !atts.containsKey(a.pos)) { atts.put(a.pos, ()); attCls(a.pos.getClass.getName) += 1 }
        }
        t match {
          case tt: TypeTree if tt.original != null => traverse(tt.original)
          case _ =>
        }
        super.traverse(t)
      }
    }
    // EmptyTree / emptyValDef etc. are singletons; fine to count them once.
    for (u <- run.units) walker.traverse(u.body)
    for ((k, v) <- treeCls) println(s"W\ttree\t$k\t$v")
    for ((k, v) <- attCls) println(s"W\tatt\t$k\t$v")
    for ((k, v) <- sharedPos) println(s"W\tshared\t$k\t$v")

    // Live heap histogram while `run` (and its units) is still reachable.
    val server = ManagementFactory.getPlatformMBeanServer
    val histo = server.invoke(new ObjectName("com.sun.management:type=DiagnosticCommand"), "gcClassHistogram",
      Array[AnyRef](Array.empty[String]), Array("[Ljava.lang.String;")).asInstanceOf[String]
    val Line = """\s*\d+:\s+(\d+)\s+(\d+)\s+(\S+).*""".r
    for (Line(n, b, c) <- histo.linesIterator) println(s"H\t$c\t$n\t$b")
    val rt = Runtime.getRuntime
    System.err.println(s"used heap after typer: ${(rt.totalMemory - rt.freeMemory) >> 20} MB")
    if (!sys.props.contains("survey.nowalk")) HeapWalk.run(global)
    if (run.units.isEmpty) println(global)
  }
}

/** Reflective identity-deduplicated walk of everything reachable from `global`, run after the histogram.
 *  For instances of Symbol, Type and Name subclasses (and the satellites they own: TypeHistory, Scope, ScopeEntry,
 *  BaseTypeSeq) it counts, per field, how many instances hold a non-default value. Default means null, zero, false,
 *  or one of a few sentinels (NoSymbol, NoType, Nil, EmptyScope, ...), which are reported separately.
 *  Arrays referenced from those objects, and from `global` itself (the name table), are summed per owning field.
 *  Prints `S\t<class>\t<n>`, `F\t<class>\t<host.field>\t<type>\t<nonDefault>\t<sentinel>=<n>,...` and
 *  `A\t<owner class>\t<host.field>\t<arrays>\t<elements>\t<bytes 12B hdr>\t<bytes 8B hdr>` rows.
 */
object HeapWalk {
  import java.lang.reflect.{Field, Modifier}
  def tracked(g: Global): Class[_] => Boolean = {
    val roots: List[Class[_]] = List(classOf[g.Symbol], classOf[g.Type], classOf[g.Name], classOf[g.Scope], classOf[g.ScopeEntry], classOf[g.BaseTypeSeq],
      Class.forName("scala.reflect.internal.Symbols$TypeHistory"))
    c => roots.exists(_.isAssignableFrom(c))
  }
  final class Info(val cls: Class[_], val fields: Array[Field], val isTracked: Boolean) {
    var n = 0L
    val nonDefault = new Array[Long](fields.length)
    val sentinel = Array.fill(fields.length)(mutable.Map[String, Long]().withDefaultValue(0L))
  }
  val skip: Set[Class[_]] = Set(classOf[Class[_]], classOf[ClassLoader], classOf[Thread], classOf[ThreadGroup], classOf[Module])
  def arrayBytes(c: Class[_], len: Int, hdr: Int): Long = {
    val e = c.getComponentType match { case java.lang.Long.TYPE | java.lang.Double.TYPE => 8; case java.lang.Integer.TYPE | java.lang.Float.TYPE => 4
      case java.lang.Short.TYPE | java.lang.Character.TYPE => 2; case java.lang.Byte.TYPE | java.lang.Boolean.TYPE => 1; case _ => 4 }
    // 12-byte header: mark + klass + length = 16; compact: mark/klass + length = 12
    val base = if (hdr == 12) 16 else 12
    (base + e.toLong * len + 7) & ~7L
  }
  def run(g: Global): Unit = {
    val isTracked = tracked(g)
    val sentinels: List[(AnyRef, String)] = List(g.NoSymbol -> "NoSymbol", g.NoType -> "NoType", g.NoPrefix -> "NoPrefix", g.NoPosition -> "NoPosition",
      g.EmptyScope -> "EmptyScope", Nil -> "Nil", None -> "None", Set() -> "Set()", Map() -> "Map()", g.nme.EMPTY -> "nme.EMPTY", g.tpnme.EMPTY -> "tpnme.EMPTY",
      g.WildcardType -> "WildcardType", g.ErrorType -> "ErrorType", g.EmptyTree -> "EmptyTree")
    val infos = new java.util.IdentityHashMap[Class[_], Info]
    def info(c: Class[_]): Info = {
      var i = infos.get(c)
      if (i eq null) {
        val fs = mutable.ArrayBuffer[Field]()
        var k: Class[_] = c
        while (k != null) {
          for (f <- k.getDeclaredFields if !Modifier.isStatic(f.getModifiers)) if (try { f.setAccessible(true); true } catch { case _: Exception => false }) fs += f
          k = k.getSuperclass
        }
        i = new Info(c, fs.toArray, isTracked(c)); infos.put(c, i)
      }
      i
    }
    val owned = mutable.Map[(String, String), Array[Long]]()
    def own(owner: Class[_], f: Field, a: AnyRef): Unit = {
      val len = java.lang.reflect.Array.getLength(a)
      val acc = owned.getOrElseUpdate((owner.getName, f.getDeclaringClass.getName + "." + f.getName), new Array[Long](4))
      acc(0) += 1; acc(1) += len; acc(2) += arrayBytes(a.getClass, len, 12); acc(3) += arrayBytes(a.getClass, len, 8)
    }
    val bitSetElems = classOf[mutable.BitSet].getDeclaredField("elems"); bitSetElems.setAccessible(true)
    println(s"N\tnameTableSize\t${g.nameTableSize}")
    val seen = new java.util.IdentityHashMap[AnyRef, java.lang.Boolean](1 << 24)
    val stack = new java.util.ArrayDeque[AnyRef](1 << 16)
    def push(o: AnyRef): Unit = if ((o ne null) && !seen.containsKey(o)) { seen.put(o, java.lang.Boolean.TRUE); stack.push(o) }
    push(g)
    var visited = 0L
    while (!stack.isEmpty) {
      val o = stack.pop(); visited += 1
      val c = o.getClass
      if (c.isArray) {
        if (!c.getComponentType.isPrimitive) { val a = o.asInstanceOf[Array[AnyRef]]; var j = 0; while (j < a.length) { push(a(j)); j += 1 } }
      } else if (!skip.exists(_.isAssignableFrom(c))) {
        val i = info(c)
        val trackFields = i.isTracked || (o eq g)
        if (i.isTracked) i.n += 1
        var j = 0
        while (j < i.fields.length) {
          val f = i.fields(j); val t = f.getType
          if (t.isPrimitive) {
            if (i.isTracked) {
              val nd = t match {
                case java.lang.Long.TYPE => f.getLong(o) != 0L; case java.lang.Integer.TYPE => f.getInt(o) != 0; case java.lang.Boolean.TYPE => f.getBoolean(o)
                case java.lang.Short.TYPE => f.getShort(o) != 0; case java.lang.Character.TYPE => f.getChar(o) != 0; case java.lang.Byte.TYPE => f.getByte(o) != 0
                case java.lang.Double.TYPE => f.getDouble(o) != 0d; case java.lang.Float.TYPE => f.getFloat(o) != 0f }
              if (nd) i.nonDefault(j) += 1
            }
          } else {
            val v = f.get(o)
            if (i.isTracked && (v ne null)) {
              sentinels.find(_._1 eq v) match {
                case Some((_, nm)) => i.sentinel(j)(nm) += 1
                case None => i.nonDefault(j) += 1
              }
            }
            if (trackFields && (v ne null) && v.getClass.isArray) own(c, f, v)
            if (i.isTracked && v.isInstanceOf[mutable.BitSet]) own(c, f, bitSetElems.get(v)) // BaseTypeSeq.pending's words
            push(v)
          }
          j += 1
        }
      }
    }
    System.err.println(s"walk visited $visited objects")
    for (i <- infos.values.asScala if i.isTracked && i.n > 0) {
      println(s"S\t${i.cls.getName}\t${i.n}")
      for (j <- i.fields.indices) {
        val f = i.fields(j)
        val sen = i.sentinel(j).toList.sortBy(-_._2).map { case (k, v) => s"$k=$v" }.mkString(",")
        println(s"F\t${i.cls.getName}\t${f.getDeclaringClass.getName}.${f.getName}\t${f.getType.getName}\t${i.nonDefault(j)}\t$sen")
      }
    }
    for (((oc, f), a) <- owned) println(s"A\t$oc\t$f\t${a(0)}\t${a(1)}\t${a(2)}\t${a(3)}")
  }
}
