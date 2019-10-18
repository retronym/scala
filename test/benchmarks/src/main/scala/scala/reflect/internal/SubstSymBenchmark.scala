/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.reflect.internal

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra._

import scala.tools.nsc.Global

@BenchmarkMode(Array(org.openjdk.jmh.annotations.Mode.AverageTime))
@Fork(2)
@Threads(1)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Benchmark)
class SubstSymBenchmark {
  import scala.tools.nsc._
  var data: SubstSymBenchmark.Data = _

  @Setup(Level.Trial)
  def setup(): Unit = {
    val settings = new Settings()
    settings.usejavacp.value = true
    settings.stopAfter.value = List("typer")
    settings.uniqid.value = true
    val global = new Global(settings)
    new global.Run()
    val Map_Class = global.symbolOf[Map[_, _]]
    val types = Map_Class.info.members.map(sym => Map_Class.typeOfThis.memberInfo(sym)).toArray
    data = new SubstSymBenchmark.Data(global)(types, Map_Class.typeParams, Map_Class.typeParams.map(_.cloneSymbol))
  }

  @Benchmark def substSymBenchmark(bh: Blackhole): Int = {
    val data = this.data
    import data._
    var i = 0
    var changed = 0
    while (i < types.length) {
      val substituted = substSym.apply(types(i))
      if (substituted ne types(i)) changed += 1
      i += 1
    }
    assert(changed > 0)
    changed
  }

  @Benchmark def substSymNoOpBenchmark(bh: Blackhole): Int = {
    val data = this.data
    import data._
    var i = 0
    var changed = 0
    while (i < types.length) {
      val substituted = substSymNoOp.apply(types(i))
      if (substituted ne types(i)) changed += 1
      i += 1
    }
    assert(changed == 0)
    changed
  }
}

object SubstSymBenchmark {
  class Data(val g: Global)(_types: Array[_ <: Global#Type], _tparams: List[Global#Symbol], _tparams2: List[Global#Symbol]) {
    import g._
    def types: Array[Type] = _types.asInstanceOf[Array[Type]]
    def tparams: List[Symbol] = _tparams.asInstanceOf[List[Symbol]]
    def tparams2: List[Symbol] = _tparams2.asInstanceOf[List[Symbol]]
    val substSym = new SubstSymMap(tparams, tparams2)
    val substSymNoOp = new SubstSymMap(tparams2, tparams)
  }
}