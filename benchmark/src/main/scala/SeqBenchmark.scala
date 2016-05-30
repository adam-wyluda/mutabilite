package benchmark

import org.openjdk.jmh.annotations._
import offheap.collection._
import org.openjdk.jmh.infra.Blackhole

import scala.util.Random
import scala.collection.mutable.{ArrayBuffer => StdlibSeq}

@State(Scope.Thread)
class SeqBenchmark {

  val seq: Seq[Int] = {
    val seq = new Seq[Int]
    1 to 10000 foreach (seq.append(_))
    seq
  }

  val stdSeq: StdlibSeq[Int] = {
    val seq = StdlibSeq[Int]()
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
  def readSequentialStdlib(blackhole: Blackhole) = {
    var i = 0
    val size = stdSeq.size
    while (i < size) {
      blackhole.consume(stdSeq(i))
      i += 1
    }
  }

  @Benchmark
  def readRandom(blackhole: Blackhole) = {
    blackhole.consume(seq(random.nextInt(seq.size)))
  }

  @Benchmark
  def readRandomStdlib(blackhole: Blackhole) = {
    blackhole.consume(stdSeq(random.nextInt(stdSeq.size)))
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
  def appendStdlib() = {
    val s = StdlibSeq[Int]()
    var i = 0
    while (i < 100000) {
      s.append(i)
      i += 1
    }
  }

  @Benchmark
  def updateSequential() = {
    val s = seq
    val size = s.size
    var i = 0
    while (i < size) {
      s(i) = i * 2
      i += 1
    }
  }

  @Benchmark
  def updateSequentialStdlib() = {
    val s = stdSeq
    val size = s.size
    var i = 0
    while (i < size) {
      s(i) = i * 2
      i += 1
    }
  }

  @Benchmark
  def updateRandom() = {
    val s = seq
    val size = s.size
    var i = 0
    while (i < size) {
      s(random.nextInt(size)) = i * 2
      i += 1
    }
  }

  @Benchmark
  def updateRandomStdlib() = {
    val s = stdSeq
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
  def foreachStdlib(blackhole: Blackhole) =
    stdSeq foreach (blackhole.consume(_))

  @Benchmark
  def prepend() = {
    val s = new Seq[Int]
    var i = 0
    while (i < 10000) {
      s.insert(0, i)
      i += 1
    }
  }

  @Benchmark
  def prependStdlib() = {
    val s = StdlibSeq[Int]()
    var i = 0
    while (i < 10000) {
      s.insert(0, i)
      i += 1
    }
  }
}

@State(Scope.Thread)
class SeqRemoveBenchmark {

  val origin: Seq[Int] = {
    val seq = new Seq[Int]
    1 to 10000 foreach (seq.append(_))
    seq
  }

  var seq: Seq[Int] = _

  @Setup(Level.Invocation)
  def setup = {
    seq = new Seq[Int]
    seq.append(origin)
  }

  @Benchmark
  def benchmark = {
    while (seq.nonEmpty) seq.remove(0)
  }
}

@State(Scope.Thread)
class SeqRemoveStdlibBenchmark {

  val origin: StdlibSeq[Int] = {
    val seq = StdlibSeq[Int]()
    1 to 10000 foreach (seq.append(_))
    seq
  }

  var seq: StdlibSeq[Int] = _

  @Setup(Level.Invocation)
  def setup = {
    seq = StdlibSeq[Int](origin: _*)
  }

  @Benchmark
  def benchmark = {
    while (seq.nonEmpty) seq.remove(0)
  }
}
