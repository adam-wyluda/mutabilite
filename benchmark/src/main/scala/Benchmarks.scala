package benchmark

import org.openjdk.jmh.annotations._
import offheap.collection._
import org.openjdk.jmh.infra.Blackhole

import scala.util.Random
import scala.{collection => stdlib}

@State(Scope.Thread)
class StdlibSeqBenchmark {

  val seq: stdlib.mutable.ArrayBuffer[Int] = {
    val seq = stdlib.mutable.ArrayBuffer[Int]()
    1 to 10000 foreach (seq.append(_))
    seq
  }

  val updatedSeq: stdlib.mutable.ArrayBuffer[Int] = {
    val seq = stdlib.mutable.ArrayBuffer[Int]()
    1 to 10000 foreach (seq.append(_))
    seq
  }

  val random = Random

  @Benchmark
  def readSequential(blackhole: Blackhole) = {
    var i = 0
    val size = seq.size
    while (i < size) {
      blackhole.consume(seq(i))
      i += 1
    }
  }

  @Benchmark
  def readRandom(blackhole: Blackhole) = {
    blackhole.consume(seq(random.nextInt(seq.size)))
  }

  @Benchmark
  def append() = {
    val s = stdlib.mutable.ArrayBuffer[Int]()
    var i = 0
    while (i < 100000) {
      s.append(i)
      i += 1
    }
  }

  @Benchmark
  def updateSequential() = {
    val s = updatedSeq
    val size = s.size
    var i = 0
    while (i < size) {
      s(i) = i * 2
      i += 1
    }
  }

  @Benchmark
  def updateRandom() = {
    val s = updatedSeq
    val size = s.size
    var i = 0
    while (i < size) {
      s(random.nextInt(size)) = i * 2
      i += 1
    }
  }

  @Benchmark
  def foreach(blackhole: Blackhole) = seq foreach (blackhole.consume(_))

  @Benchmark
  def remove() = {
    val s = stdlib.mutable.ArrayBuffer[Int](seq: _*)
    while (s.nonEmpty) s.remove(0)
  }

  @Benchmark
  def prepend() = {
    val s = stdlib.mutable.ArrayBuffer[Int]()
    var i = 0
    while (i < 10000) {
      s.insert(0, i)
      i += 1
    }
  }
}

@State(Scope.Thread)
class SeqBenchmark {

  val seq: Seq[Int] = {
    val seq = new Seq[Int]
    1 to 10000 foreach (seq.append(_))
    seq
  }

  val updatedSeq: Seq[Int] = {
    val seq = new Seq[Int]
    1 to 10000 foreach (seq.append(_))
    seq
  }

  val random = Random

  @Benchmark
  def readSequential(blackhole: Blackhole) = {
    var i = 0
    val size = seq.size
    while (i < size) {
      blackhole.consume(seq(i))
      i += 1
    }
  }

  @Benchmark
  def readRandom(blackhole: Blackhole) = {
    blackhole.consume(seq(random.nextInt(seq.size)))
  }

  @Benchmark
  def append() = {
    val s = new Seq[Int](initialSize = 16)
    var i = 0
    while (i < 100000) {
      s.append(i)
      i += 1
    }
  }

  @Benchmark
  def updateSequential() = {
    val s = updatedSeq
    val size = s.size
    var i = 0
    while (i < size) {
      s(i) = i * 2
      i += 1
    }
  }

  @Benchmark
  def updateRandom() = {
    val s = updatedSeq
    val size = s.size
    var i = 0
    while (i < size) {
      s(random.nextInt(size)) = i * 2
      i += 1
    }
  }

  @Benchmark
  def foreach(blackhole: Blackhole) = seq foreach (blackhole.consume(_))

  @Benchmark
  def remove() = {
    val s = new Seq[Int]
    s.append(seq)
    while (s.nonEmpty) s.remove(0)
  }

  @Benchmark
  def prepend() = {
    val s = new Seq[Int]
    var i = 0
    while (i < 10000) {
      s.insert(0, i)
      i += 1
    }
  }
}
