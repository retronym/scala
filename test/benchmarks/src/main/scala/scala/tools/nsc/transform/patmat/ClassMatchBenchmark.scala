package scala.tools.nsc.transform.patmat

import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations.CompilerControl.Mode.DONT_INLINE
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

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

  private var names: Array[Name] = null

  @Setup def setup(): Unit = {
    val r = new Random(12345)
    val names = Array[Name](Name0(), Name1(), Name2(), Name3(), Name4(), Name5(), Name6(), Name7(), Name8(), Name9(), Name10(), Name11(), Name12(), Name13(), Name14(), Name15())
    this.names = Array.fill(count)(names(r.nextInt(names.length)))
  }

  @Benchmark def patmatShow(bh: Blackhole): Unit    = {
    val names = this.names
    var i = 0
    while (i < names.length) {
      val x = names(i) match {
        case Name0() => "0"
        case Name1() => "1"
        case Name2() => "2"
        case Name3() => "3"
        case Name4() => "4"
        case Name5() => "5"
        case Name6() => "6"
        case Name7() => "7"
        case Name8() => "8"
        case Name9() => "9"
        case Name10() => "10"
        case Name11() => "11"
        case Name12() => "12"
        case Name13() => "13"
        case Name14() => "14"
        case Name15() => "15"
      }
      bh.consume(x)
      i += 1
    }
  }
  @Benchmark def virtualShow(bh: Blackhole): Unit   = {
    val names = this.names
    var i = 0
    while (i < names.length) {
      bh.consume(names(i).virtualShow)
      i += 1
    }
  }
  @Benchmark def intSwitchShow(bh: Blackhole): Unit = {
    val names = this.names
    var i = 0
    while (i < names.length) {
      val x = (names(i)._id: @switch) match {
        case 0 => "0"
        case 1 => "1"
        case 2 => "2"
        case 3 => "3"
        case 4 => "4"
        case 5 => "5"
        case 6 => "6"
        case 7 => "7"
        case 8 => "8"
        case 9 => "9"
        case 10 => "10"
        case 11 => "11"
        case 12 => "12"
        case 13 => "13"
        case 14 => "14"
        case 15 => "15"
      }
      bh.consume(x)
      i += 1
    }
  }
}

sealed abstract class Name(val _id: Int) {
  def virtualShow: String
}

final case class Name0() extends Name(0) { def virtualShow = "0" }
final case class Name1() extends Name(1) { def virtualShow = "1" }
final case class Name2() extends Name(2) { def virtualShow = "2" }
final case class Name3() extends Name(3) { def virtualShow = "3" }
final case class Name4() extends Name(4) { def virtualShow = "4" }
final case class Name5() extends Name(5) { def virtualShow = "5" }
final case class Name6() extends Name(6) { def virtualShow = "6" }
final case class Name7() extends Name(7) { def virtualShow = "7" }
final case class Name8() extends Name(8) { def virtualShow = "8" }
final case class Name9() extends Name(9) { def virtualShow = "9" }
final case class Name10() extends Name(10) { def virtualShow = "10" }
final case class Name11() extends Name(11) { def virtualShow = "11" }
final case class Name12() extends Name(12) { def virtualShow = "12" }
final case class Name13() extends Name(13) { def virtualShow = "13" }
final case class Name14() extends Name(14) { def virtualShow = "14" }
final case class Name15() extends Name(15) { def virtualShow = "15" }
