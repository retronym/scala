package scala.tools.nsc.transform.patmat

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations.CompilerControl.Mode.DONT_INLINE
import org.openjdk.jmh.annotations._

import scala.annotation.switch
import scala.util.Random

@Warmup(iterations = 10, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 10, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(3)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@State(Scope.Benchmark)
class ClassMatchBenchmark {
  @Param(Array("10000")) private var count = 0
  @Param(Array("1"))     private var p1    = 0
  @Param(Array("1"))     private var p2    = 0
  @Param(Array("1"))     private var p3    = 0

  private var names: Array[Name] = null

  @Setup def setup(): Unit = {
    val r = new Random(12345)
    val s1 = (count *  p1)       / (p1 + p2 + p3)
    val s2 = (count * (p1 + p2)) / (p1 + p2 + p3)
    val ns = for (c <- 0 until count) yield
      if (c < s1) Name0("foo", "bar") else if (c < s2) Name1("foo", "bar") else Name2("foo", "bar")
    names = r.shuffle(ns).toArray
  }

  @Benchmark def patmatShow: Unit    = names.foreach(_.patmatShow)
  @Benchmark def virtualShow: Unit   = names.foreach(_.virtualShow)
  @Benchmark def intSwitchShow: Unit = names.foreach(_.intSwitchShow)
}

sealed trait Name {
  @CompilerControl(DONT_INLINE) final def patmatShow: String = this match {
    case Name0(prefix, name) => s"0-$prefix$name"
    case Name1(prefix, name) => s"1-$prefix$name"
    case Name2(prefix, name) => s"2-$prefix$name"
  }

  @CompilerControl(DONT_INLINE) def virtualShow: String

  protected[this] def _id: Int
  def prefix: String
  def name: String
  @CompilerControl(DONT_INLINE) final def intSwitchShow: String = (_id: @switch) match {
    case 0 => s"0-$prefix$name"
    case 1 => s"1-$prefix$name"
    case 2 => s"2-$prefix$name"
  }
}

final case class Name0(prefix: String, name: String) extends Name { protected[this] def _id = 0; def virtualShow = s"0-$prefix$name" }
final case class Name1(prefix: String, name: String) extends Name { protected[this] def _id = 1; def virtualShow = s"1-$prefix$name" }
final case class Name2(prefix: String, name: String) extends Name { protected[this] def _id = 2; def virtualShow = s"2-$prefix$name" }
