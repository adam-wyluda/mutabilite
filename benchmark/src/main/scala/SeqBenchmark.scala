package benchmark

import org.openjdk.jmh.annotations._
import offheap.collection._
import HashEq.Implicits._
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

  var randIndex: Int = _
  var randVal: Int = _

  @Setup(Level.Invocation)
  def setup = {
    randIndex = random.nextInt(seqSize)
    randVal = random.nextInt
  }

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
  def readRandom = seq(randIndex)

  @Benchmark
  def readRandomStdlib = stdSeq(randIndex)

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
  def updateRandom = seq(randIndex) = randVal

  @Benchmark
  def updateRandomStdlib = stdSeq(randIndex) = randVal

  @Benchmark
  def foreach = {
    var sum = 0
    seq foreach (sum += _)
    sum
  }

  @Benchmark
  def foreachStdlib = {
    var sum = 0
    stdSeq foreach (sum += _)
    sum
  }

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
