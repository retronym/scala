package w4d

import java.io.File
import java.net.URLClassLoader
import java.util.concurrent._
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger}
import scala.jdk.CollectionConverters._

/** What one configuration did with one program. kind: ok, compile-error, crash, timeout-compile, timeout-run, run-throw. */
case class Outcome(kind: String, text: String) {
  def compiled: Boolean = kind == "ok" || kind == "run-throw" || kind == "timeout-run"
}

/** A scalac 2.13 in its own class loader (parent: the platform loader), reached through W4Bridge compiled by itself.
 *  `libJar` is that compiler's scala-library; compiled programs run in a loader whose parent holds only that jar. */
class Compiler(val name: String, jars: Seq[String], bridgeDir: String, val libJar: String) {
  private def url(p: String) = new File(p).toURI.toURL
  val loader = new URLClassLoader((jars :+ bridgeDir).map(url).toArray, ClassLoader.getPlatformClassLoader)
  private val m = loader.loadClass("scala.tools.nsc.W4Bridge").getMethod("compile",
    classOf[Array[String]], classOf[Array[String]], classOf[Array[String]], classOf[String], classOf[java.util.Map[_, _]])
  val libLoader = new URLClassLoader(Array(url(libJar)), ClassLoader.getPlatformClassLoader)
  /** Left(errors) or Right(class files by path). */
  def compile(units: Seq[(String, String)], flags: Seq[String], prior: java.util.Map[String, Array[Byte]]): Either[String, java.util.Map[String, Array[Byte]]] = {
    val t = Thread.currentThread; val old = t.getContextClassLoader
    t.setContextClassLoader(loader)
    try {
      val r = m.invoke(null, units.map(_._1).toArray, units.map(_._2).toArray, flags.toArray, libJar, prior).asInstanceOf[Array[AnyRef]]
      if (r(0) != null) Left(r(0).asInstanceOf[String]) else Right(r(1).asInstanceOf[java.util.Map[String, Array[Byte]]])
    } finally t.setContextClassLoader(old)
  }
}
object Compiler {
  /** `pack` is a build/pack directory or a colon-separated list of compiler, reflect and library jars. */
  def apply(name: String, pack: String, bridgeDir: String): Compiler = {
    val jars = if (new File(pack, "lib").isDirectory) List("scala-compiler", "scala-reflect", "scala-library").map(j => s"$pack/lib/$j.jar")
               else pack.split(File.pathSeparator).filter(_.nonEmpty).toList
    new Compiler(name, jars, bridgeDir, jars.find(_.contains("scala-library")).get)
  }
}

/** A way to compile a program: which compiler, which flags, joint or one unit at a time in order, and a source rewrite. */
case class Config(name: String, compiler: Compiler, flags: List[String] = Nil, separate: Boolean = false, rewrite: String => String = identity)

/** Loads compiled classes from memory. */
class MapLoader(classes: java.util.Map[String, Array[Byte]], parent: ClassLoader) extends ClassLoader(parent) {
  override def findClass(n: String): Class[_] = {
    val b = classes.get(n.replace('.', '/') + ".class")
    if (b == null) throw new ClassNotFoundException(n) else defineClass(n, b, 0, b.length)
  }
}

/** Runs work with a time limit on at most `threads` threads at once. A timed-out task's thread cannot be killed;
 *  it is abandoned and counted in `stuck`, and gives back its permit so the pool keeps its width. */
class Pool(threads: Int) {
  private val exec = Executors.newCachedThreadPool { (r: Runnable) => val t = new Thread(null, r, "w4d", 32L << 20); t.setDaemon(true); t }
  private val permits = new Semaphore(threads)
  val stuck = new AtomicInteger
  def timed[A](secs: Int)(f: => A): Option[A] = {
    permits.acquire()
    val given = new AtomicBoolean(false)
    def release(): Unit = if (given.compareAndSet(false, true)) permits.release()
    val fut = exec.submit(new Callable[A] { def call(): A = try f finally release() })
    try Some(fut.get(secs.toLong, TimeUnit.SECONDS))
    catch {
      case _: TimeoutException => fut.cancel(true); stuck.incrementAndGet(); release(); None
      case e: ExecutionException => throw e.getCause
    }
  }
  /** Runs the thunks concurrently (each may call `timed`) and returns their results in order. */
  def all[A](fs: Seq[() => A]): Seq[A] = fs.map(f => exec.submit(new Callable[A] { def call(): A = f() })).map(_.get)
}

class Driver(val pool: Pool, compileSecs: Int = 60, runSecs: Int = 20, entry: String = "Main", method: String = "run") {
  val compiles = new AtomicInteger
  val times = new ConcurrentHashMap[String, java.lang.Long]()

  /** Compiles `units` (dependency order) under `c` and runs `entry.method()`, which must return a String. */
  def eval(units: List[String], c: Config): Outcome = {
    val t0 = System.nanoTime
    try eval0(units.map(c.rewrite), c) finally times.merge(c.name, (System.nanoTime - t0) / 1000000, (a, b) => a + b)
  }
  private def eval0(us: List[String], c: Config): Outcome = {
    val res = pool.timed(compileSecs) {
      if (!c.separate) { compiles.incrementAndGet(); c.compiler.compile(List("p.scala" -> us.mkString("\n")), c.flags, java.util.Collections.emptyMap()) }
      else us.zipWithIndex.foldLeft[Either[String, java.util.Map[String, Array[Byte]]]](Right(new java.util.HashMap)) {
        case (Right(acc), (u, i)) =>
          compiles.incrementAndGet()
          c.compiler.compile(List(s"u$i.scala" -> u), c.flags, acc).map { m => val all = new java.util.HashMap(acc); all.putAll(m); all }
        case (left, _) => left
      }
    }
    res match {
      case None => Outcome("timeout-compile", "")
      case Some(Left(e)) => Outcome(if (e.startsWith("CRASH")) "crash" else "compile-error", e)
      case Some(Right(classes)) => run(new MapLoader(classes, c.compiler.libLoader))
    }
  }
  def run(loader: ClassLoader): Outcome = pool.timed(runSecs) {
    try Outcome("ok", loader.loadClass(entry).getMethod(method).invoke(null).asInstanceOf[String])
    catch {
      case e: java.lang.reflect.InvocationTargetException => Outcome("run-throw", e.getCause.getClass.getName)
      case e: Throwable => Outcome("run-throw", e.getClass.getName)
    }
  }.getOrElse(Outcome("timeout-run", ""))

  /** Every configuration of one program, concurrently. */
  def evalAll(units: List[String], cs: Seq[Config]): Seq[(Config, Outcome)] = cs.zip(pool.all(cs.map(c => () => eval(units, c))))

  def timesReport: String = times.asScala.toList.sortBy(_._1).map { case (k, v) => s"$k=${v}ms" }.mkString(" ")
}
