package benchmark

import org.openjdk.jmh.annotations._
import offheap.collection._
import HashEq.Implicits._
import org.openjdk.jmh.infra.Blackhole

import scala.util.Random
import scala.collection.mutable.{ArrayBuffer => StdlibSeq}

@State(Scope.Thread)
class SeqBenchmark {

  val seqSize = Benchmark.size

//  implicit val props = Region.Props(Pool(pageSize = 8 * 1024 * 1024, chunkSize = 16 * 1024 * 1024))
//  val malloc = scala.offheap.malloc

//  val offheapSeq: OffheapBufferSeq_Int = {
//    implicit val alloc = malloc
//    val seq = OffheapSeq_Int.create()
//    1 to seqSize foreach (seq.append(_))
//    seq
//  }

  val specSeq: BufferSeq_Int = {
    val seq = new BufferSeq_Int
    1 to seqSize foreach (seq.append(_))
    seq
  }

  val genericSeq: BufferSeq[Int] = {
    val seq = new BufferSeq[Int]
    1 to seqSize foreach (seq.append(_))
    seq
  }

  val stdSeq: StdlibSeq[Int] = {
    val seq = StdlibSeq[Int]()
    1 to seqSize foreach (seq.append(_))
    seq
  }

//  var freedSeq: OffheapBufferSeq_Int = _

  val random = Random

  var randIndex: Int = _
  var randVal: Int = _

//  var region: Region = _

  @Setup(Level.Invocation)
  def setup = {
    randIndex = random.nextInt(seqSize)
    randVal = random.nextInt
//    freedSeq = OffheapBufferSeq_Int.empty
//    region = Region.open
  }

//  @TearDown(Level.Invocation)
//  def tearDown = {
//    if (freedSeq.nonEmpty) freedSeq.free(malloc)
//    region.close
//  }

//  @Benchmark
//  def readSequentialOffheap(blackhole: Blackhole) = {
//    var i = 0
//    while (i < seqSize) {
//      blackhole.consume(offheapSeq(i))
//      i += 1
//    }
//  }

  @Benchmark
  def readSequentialSpecialized(blackhole: Blackhole) = {
    var i = 0
    while (i < seqSize) {
      blackhole.consume(specSeq(i))
      i += 1
    }
  }

  @Benchmark
  def readSequentialGeneric(blackhole: Blackhole) = {
    var i = 0
    while (i < seqSize) {
      blackhole.consume(genericSeq(i))
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

//  @Benchmark
//  def readRandomOffheap = offheapSeq(randIndex)

  @Benchmark
  def readRandomSpecialized = specSeq(randIndex)

  @Benchmark
  def readRandomGeneric = genericSeq(randIndex)

  @Benchmark
  def readRandomStdlib = stdSeq(randIndex)

//  @Benchmark
//  def appendOffheap = {
//    implicit val alloc = malloc
//    freedSeq = OffheapSeq_Int.create(initialSize = 16)
//    var i = 0
//    while (i < seqSize) {
//      freedSeq.append(i)
//      i += 1
//    }
//  }
//
//  @Benchmark
//  def appendRegion = {
//    implicit val alloc = region
//    val s = OffheapSeq_Int.create(initialSize = 16)
//    var i = 0
//    while (i < seqSize) {
//      s.append(i)
//      i += 1
//    }
//  }


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
  def appendGeneric = {
    val s = new BufferSeq[Int](initialSize = 16)
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

//  @Benchmark
//  def updateSequentialOffheap = {
//    val s = offheapSeq
//    var i = 0
//    while (i < seqSize) {
//      s(i) = i * 2
//      i += 1
//    }
//  }

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
  def updateSequentialGeneric = {
    val s = genericSeq
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

//  @Benchmark
//  def updateRandomOffheap = offheapSeq(randIndex) = randVal

  @Benchmark
  def updateRandomSpecialized = specSeq(randIndex) = randVal

  @Benchmark
  def updateRandomGeneric = genericSeq(randIndex) = randVal

  @Benchmark
  def updateRandomStdlib = stdSeq(randIndex) = randVal

//  @Benchmark
//  def foreachOffheap = {
//    var sum = 0
//    offheapSeq foreach (sum += _)
//    sum
//  }

  @Benchmark
  def foreachSpecialized = {
    var sum = 0
    specSeq foreach (sum += _)
    sum
  }

  @Benchmark
  def foreachGeneric = {
    var sum = 0
    genericSeq foreachGeneric (sum += _)
    sum
  }

  @Benchmark
  def foreachStdlib = {
    var sum = 0
    stdSeq foreach (sum += _)
    sum
  }

//  @Benchmark
//  def prependOffheap = {
//    implicit val alloc = malloc
//    freedSeq = OffheapSeq_Int.create(initialSize = 16)
//    var i = 0
//    while (i < seqSize) {
//      freedSeq.insert(0, i)
//      i += 1
//    }
//  }
//
//  @Benchmark
//  def prependRegion = {
//    implicit val alloc = region
//    val s = OffheapSeq_Int.create(initialSize = 16)
//    var i = 0
//    while (i < seqSize) {
//      s.insert(0, i)
//      i += 1
//    }
//  }

  @Benchmark
  def prependSpecialized = {
    val s = new BufferSeq_Int(initialSize = 16)
    var i = 0
    while (i < seqSize) {
      s.insert(0, i)
      i += 1
    }
  }

  @Benchmark
  def prependGeneric = {
    val s = new BufferSeq[Int](initialSize = 16)
    var i = 0
    while (i < seqSize) {
      s.insert(0, i)
      i += 1
    }
  }

  @Benchmark
  def prependStdlib = {
    val s = new StdlibSeq[Int](initialSize = 16)
    var i = 0
    while (i < seqSize) {
      s.insert(0, i)
      i += 1
    }
  }

//  @Benchmark
//  def mapOffheap = freedSeq = offheapSeq.map_Int(_ + 1)(malloc)
//
//  @Benchmark
//  def mapRegion = offheapSeq.map_Int(_ + 1)(region)

  @Benchmark
  def mapSpecialized = specSeq map (_ + 1)

  @Benchmark
  def mapSpecOld = specSeq map_Int (_ + 1)

  @Benchmark
  def mapGeneric = genericSeq map (_ + 1)

  @Benchmark
  def mapStdlib = stdSeq map (_ + 1)

//  @Benchmark
//  def flatMapOffheap = freedSeq = {
//    implicit val alloc = malloc
//    offheapSeq flatMap_Int { i =>
//      val r = OffheapSeq_Int.create()
//      var j = 0
//      while (j < 5) { r.append(i + j); j += 1 }
//      r
//    }
//  }
//
//  @Benchmark
//  def flatMapRegion = {
//    implicit val alloc = region
//    offheapSeq flatMap_Int { i =>
//      val s = OffheapSeq_Int.create()
//      var j = 0
//      while (j < 5) { s.append(i + j); j += 1 }
//      s
//    }
//  }

  @Benchmark
  def flatMapSpecialized =
    specSeq flatMap { i =>
      val r = new BufferSeq_Int
      var j = 0
      while (j < 5) { r.append(i + j); j += 1 }
      r
    }

  @Benchmark
  def flatMapSpecOld =
    specSeq flatMap_Int { i =>
      val r = new BufferSeq_Int
      var j = 0
      while (j < 5) { r.append(i + j); j += 1 }
      r
    }

  @Benchmark
  def flatMapGeneric =
    genericSeq flatMap { i =>
      val r = new BufferSeq[Int]
      var j = 0
      while (j < 5) { r.append(i + j); j += 1 }
      r
    }

  @Benchmark
  def flatMapStdlib =
    stdSeq flatMap { i =>
      val r = StdlibSeq[Int]()
      var j = 0
      while (j < 5) { r.append(i + j); j += 1 }
      r
    }

//  @Benchmark
//  def filterOffheap = freedSeq = offheapSeq.filter(_ % 2 == 0)(malloc)

//  @Benchmark
//  def filterRegion = offheapSeq.filter(_ % 2 == 0)(region)

  @Benchmark
  def filterSpecialized = specSeq filter (_ % 2 == 0)

  @Benchmark
  def filterSpecOld = specSeq filter_Int (_ % 2 == 0)

  @Benchmark
  def filterGeneric = genericSeq filter (_ % 2 == 0)

  @Benchmark
  def filterStdlib = stdSeq filter (_ % 2 == 0)
}

//@State(Scope.Thread)
//class SeqRemoveOffheapBenchmark {
//
//  implicit val allocator = scala.offheap.malloc
//
//  val origin: OffheapBufferSeq_Int = {
//    val seq = OffheapSeq_Int.create()
//    1 to 10000 foreach (seq.append(_))
//    seq
//  }
//
//  var seq: OffheapBufferSeq_Int = _
//
//  @Setup(Level.Invocation)
//  def setup = {
//    seq = OffheapSeq_Int.create()
//    origin.foreach(seq.append(_))
//  }
//
//  @TearDown(Level.Invocation)
//  def tearDown = seq.free
//
//  @Benchmark
//  def benchmark = {
//    while (seq.notEmpty) seq.remove(0)
//  }
//}

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
    while (seq.notEmpty) seq.remove(0)
  }
}

@State(Scope.Thread)
class SeqRemoveGenericBenchmark {

  val origin: BufferSeq[Int] = {
    val seq = new BufferSeq[Int]
    1 to 10000 foreach (seq.append(_))
    seq
  }

  var seq: BufferSeq[Int] = _

  @Setup(Level.Invocation)
  def setup = {
    seq = new BufferSeq[Int]
    seq.append(origin)
  }

  @Benchmark
  def benchmark = {
    while (seq.notEmpty) seq.remove(0)
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
