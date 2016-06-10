package benchmark

import org.openjdk.jmh.annotations._
import offheap.collection.{IntBufferSeq, _}
import org.openjdk.jmh.infra.Blackhole

import scala.util.Random
import scala.collection.mutable.{ArrayBuffer => StdlibSeq}

@State(Scope.Thread)
class SeqBenchmark {

  val seqSize = 10000

  val seq: IntBufferSeq = {
    val seq = new IntBufferSeq
    1 to seqSize foreach (seq.append(_))
    seq
  }

  val stdSeq: StdlibSeq[Int] = {
    val seq = StdlibSeq[Int]()
    1 to seqSize foreach (seq.append(_))
    seq
  }

  val random = Random

  @Benchmark
  def readSequential(blackhole: Blackhole) = {
    var i = 0
    while (i < seqSize) {
      blackhole.consume(seq(i))
      i += 1
    }
  }

  @Benchmark
  def readSequentialStdlib(blackhole: Blackhole) = {
    var i = 0
    while (i < seqSize) {
      blackhole.consume(stdSeq(i))
      i += 1
    }
  }

  @Benchmark
  def readRandom(blackhole: Blackhole) = {
    blackhole.consume(seq(random.nextInt(seqSize)))
  }

  @Benchmark
  def readRandomStdlib(blackhole: Blackhole) = {
    blackhole.consume(stdSeq(random.nextInt(seqSize)))
  }

  @Benchmark
  def append() = {
    val s = new IntBufferSeq(initialSize = 16)
    var i = 0
    while (i < seqSize) {
      s.append(i)
      i += 1
    }
  }

  @Benchmark
  def appendStdlib() = {
    val s = new StdlibSeq[Int](initialSize = 16)
    var i = 0
    while (i < seqSize) {
      s.append(i)
      i += 1
    }
  }

  @Benchmark
  def updateSequential() = {
    val s = seq
    var i = 0
    while (i < seqSize) {
      s(i) = i * 2
      i += 1
    }
  }

  @Benchmark
  def updateSequentialStdlib() = {
    val s = stdSeq
    var i = 0
    while (i < seqSize) {
      s(i) = i * 2
      i += 1
    }
  }

  @Benchmark
  def updateRandom() = {
    val s = seq
    var i = 0
    while (i < seqSize) {
      s(random.nextInt(seqSize)) = i * 2
      i += 1
    }
  }

  @Benchmark
  def updateRandomStdlib() = {
    val s = stdSeq
    var i = 0
    while (i < seqSize) {
      s(random.nextInt(seqSize)) = i * 2
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
    val s = new IntBufferSeq(initialSize = 16)
    var i = 0
    while (i < seqSize) {
      s.insert(0, i)
      i += 1
    }
  }

  @Benchmark
  def prependStdlib() = {
    val s = new StdlibSeq[Int](initialSize = 16)
    var i = 0
    while (i < seqSize) {
      s.insert(0, i)
      i += 1
    }
  }
}

@State(Scope.Thread)
class SeqRemoveBenchmark {

  val origin: IntBufferSeq = {
    val seq = new IntBufferSeq
    1 to 10000 foreach (seq.append(_))
    seq
  }

  var seq: IntBufferSeq = _

  @Setup(Level.Invocation)
  def setup = {
    seq = new IntBufferSeq
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
