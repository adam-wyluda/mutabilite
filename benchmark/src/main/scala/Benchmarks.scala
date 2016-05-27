package benchmark

import org.openjdk.jmh.annotations._
import offheap.collection._
import org.openjdk.jmh.infra.Blackhole

import scala.util.Random
import scala.{collection => stdlib}

@State(Scope.Thread)
class StdlibSeqBenchmark {

  val seq: stdlib.IndexedSeq[Int] = {
    var seq = stdlib.IndexedSeq[Int]()
    1 to 10000 foreach (i => seq = seq :+ i)
    seq
  }

  val random = Random

  @Benchmark
  def readSequential(blackhole: Blackhole) = {
    var i = 0
    while (i < seq.size) {
      blackhole.consume(seq(i))
      i += 1
    }
  }

  @Benchmark
  def readRandom(blackhole: Blackhole) = {
    blackhole.consume(seq(random.nextInt(seq.size)))
  }
}

@State(Scope.Thread)
class SeqBenchmark {

  val seq: Seq[Int] = {
    val seq = new Seq[Int]
    1 to 10000 foreach (seq.append(_))
    seq
  }

  val random = Random

  @Benchmark
  def readSequential(blackhole: Blackhole) = {
    var i = 0
    while (i < seq.size) {
      blackhole.consume(seq(i))
      i += 1
    }
  }

  @Benchmark
  def readRandom(blackhole: Blackhole) = {
    blackhole.consume(seq(random.nextInt(seq.size)))
  }
}
