package benchmark

import java.util.function.Consumer

import org.openjdk.jmh.annotations._
import offheap.collection._
import org.openjdk.jmh.infra.Blackhole

import scala.util.Random
import scala.collection.mutable.{ArrayBuffer => StdlibSeq}
import java.util.{ArrayList => JavaSeq}

@State(Scope.Thread)
class SeqBenchmark {

  val seqSize = Benchmark.size

  val specSeq: BufferSeq_Int = {
    val seq = new BufferSeq_Int
    1 to seqSize foreach (seq.append(_))
    seq
  }

  val deboxSeq: debox.Buffer[Int] = {
    val seq = debox.Buffer.empty[Int]
    1 to seqSize foreach (seq.append(_))
    seq
  }

  val stdSeq: StdlibSeq[Int] = {
    val seq = StdlibSeq[Int]()
    1 to seqSize foreach (seq.append(_))
    seq
  }

  val javaSeq: JavaSeq[Int] = {
    val seq = new JavaSeq[Int]
    1 to seqSize foreach (seq.add(_))
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
  def readSequentialSpecialized(blackhole: Blackhole) = {
    var i = 0
    while (i < seqSize) {
      blackhole.consume(specSeq(i))
      i += 1
    }
  }

  @Benchmark
  def readSequentialDebox(blackhole: Blackhole) = {
    var i = 0
    while (i < seqSize) {
      blackhole.consume(deboxSeq(i))
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
  def readSequentialJavalib(blackhole: Blackhole) = {
    var i = 0
    while (i < seqSize) {
      blackhole.consume(javaSeq.get(i))
      i += 1
    }
  }

  @Benchmark
  def readRandomSpecialized = specSeq(randIndex)

  @Benchmark
  def readRandomDebox = deboxSeq(randIndex)

  @Benchmark
  def readRandomStdlib = stdSeq(randIndex)

  @Benchmark
  def readRandomJavalib = javaSeq.get(randIndex)

  @Benchmark
  def appendSpecialized = {
    val s = new BufferSeq_Int(initialSize = 16)
    var i = 0
    while (i < seqSize) {
      s.append(i)
      i += 1
    }
  }

  @Benchmark
  def appendDebox = {
    var s = debox.Buffer.ofSize[Int](16)
    var i = 0
    while (i < seqSize) {
      s.append(i)
      i += 1
    }
  }

  @Benchmark
  def appendStdlib = {
    val s = new StdlibSeq[Int](initialSize = 16)
    var i = 0
    while (i < seqSize) {
      s.append(i)
      i += 1
    }
  }

  @Benchmark
  def appendJavalib = {
    val s = new JavaSeq[Int](16)
    var i = 0
    while (i < seqSize) {
      s.add(i)
      i += 1
    }
  }

  @Benchmark
  def updateSequentialSpecialized = {
    val s = specSeq
    var i = 0
    while (i < seqSize) {
      s(i) = i * 2
      i += 1
    }
  }

  @Benchmark
  def updateSequentialDebox = {
    var s = deboxSeq
    var i = 0
    while (i < seqSize) {
      s(i) = i * 2
      i += 1
    }
  }

  @Benchmark
  def updateSequentialStdlib = {
    val s = stdSeq
    var i = 0
    while (i < seqSize) {
      s(i) = i * 2
      i += 1
    }
  }

  @Benchmark
  def updateSequentialJavalib = {
    val s = stdSeq
    var i = 0
    while (i < seqSize) {
      s(i) = i * 2
      i += 1
    }
  }

  @Benchmark
  def updateRandomSpecialized = specSeq(randIndex) = randVal

  @Benchmark
  def updateRandomDebox = deboxSeq(randIndex) = randVal

  @Benchmark
  def updateRandomStdlib = stdSeq(randIndex) = randVal

  @Benchmark
  def updateRandomJavalib = javaSeq.set(randIndex, randVal)

  @Benchmark
  def foreachMacro = {
    var sum = 0
    specSeq foreachMacro (sum += _)
    sum
  }

  @Benchmark
  def foreachSpecialized = {
    var sum = 0
    specSeq foreach (sum += _)
    sum
  }

  @Benchmark
  def foreachDebox = {
    var sum = 0
    deboxSeq foreach (sum += _)
    sum
  }

  @Benchmark
  def foreachStdlib = {
    var sum = 0
    stdSeq foreach (sum += _)
    sum
  }

  @Benchmark
  def foreachJavalib = {
    var sum = 0
    javaSeq.forEach(new Consumer[Int] {
      def accept(t: Int): Unit = sum += t
    })
    sum
  }

  @Benchmark
  def mapSpecialized = specSeq map (_ + 1)

  @Benchmark
  def mapDebox = deboxSeq map (_ + 1)

  @Benchmark
  def mapStdlib = stdSeq map (_ + 1)
}

@State(Scope.Thread)
class SeqRemoveSpecializedBenchmark {

  val origin: BufferSeq_Int = {
    val seq = new BufferSeq_Int
    1 to 10000 foreach (seq.append(_))
    seq
  }

  var seq: BufferSeq_Int = _

  @Setup(Level.Invocation)
  def setup = {
    seq = new BufferSeq_Int
    origin.foreach(seq.append(_))
  }

  @Benchmark
  def benchmark = {
    while (seq.nonEmpty) seq.remove(0)
  }
}

@State(Scope.Thread)
class SeqRemoveDeboxBenchmark {

  val origin: debox.Buffer[Int] = {
    val seq = debox.Buffer.empty[Int]
    1 to 10000 foreach (seq.append(_))
    seq
  }

  var seq: debox.Buffer[Int] = _

  @Setup(Level.Invocation)
  def setup = {
    seq = debox.Buffer.empty[Int]
    origin.foreach(seq.append(_))
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

@State(Scope.Thread)
class SeqRemoveJavalibBenchmark {

  var seq: JavaSeq[Int] = _

  @Setup(Level.Invocation)
  def setup = {
    seq = new JavaSeq[Int]()
    1 to 10000 foreach (seq.add(_))
  }

  @Benchmark
  def benchmark = {
    while (!seq.isEmpty) seq.remove(0)
  }
}
