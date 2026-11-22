package scala.reflect.internal

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._

/** Creating TypeRefs that are already unique, as `TypeRef.mapOver` mostly does. */
@BenchmarkMode(Array(Mode.AverageTime))
@Fork(3)
@Threads(1)
@Warmup(iterations = 8, time = 1)
@Measurement(iterations = 8, time = 1)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Benchmark)
class UniqueTypeRefBenchmark {
  import scala.tools.nsc._
  var g: Global = _
  var pre: Global#Type = _
  var sym: Global#Symbol = _
  var args: List[Global#Type] = _
  var tpe: Global#Type = _
  var map: Global#TypeMap = _

  @Setup(Level.Trial)
  def setup(): Unit = {
    val settings = new Settings()
    settings.usejavacp.value = true
    val global = new Global(settings)
    g = global
    new global.Run()
    import global._
    val mapTpe = typeOf[Map[String, List[Option[Int]]]]
    pre = mapTpe.prefix
    sym = mapTpe.typeSymbol
    args = mapTpe.typeArgs.map(x => x) // not the list the TypeRef holds
    // Map[A, Either[A, List[A]]] with A, a type parameter of Map, replaced by String
    val a = definitions.ListClass.typeParams.head
    tpe = appliedType(mapTpe.typeConstructor, a.tpeHK :: appliedType(rootMirror.getRequiredClass("scala.util.Either"), a.tpeHK :: appliedType(definitions.ListClass, a.tpeHK) :: Nil) :: Nil)
    map = new SubstTypeMap(a :: Nil, definitions.StringTpe :: Nil)
  }

  @Benchmark def typeRefHit(): Any = {
    val global = g
    global.TypeRef(pre.asInstanceOf[global.Type], sym.asInstanceOf[global.Symbol], args.asInstanceOf[List[global.Type]])
  }

  @Benchmark def substHits(): Any = {
    val global = g
    map.asInstanceOf[global.TypeMap].apply(tpe.asInstanceOf[global.Type])
  }
}
