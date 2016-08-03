package benchmark

import org.openjdk.jmh.annotations._
import offheap.collection._
import offheap.collection.ops._
import org.openjdk.jmh.infra.Blackhole

import scala.collection.mutable.{Buffer => StdlibBuffer, OpenHashMap => StdlibMap}

@State(Scope.Thread)
class IntMapBenchmark {

  import Benchmark._

  val specMap: HashMap_Int_Int = {
    val map = new HashMap_Int_Int(initialSize)
    var i = 0
    while (i < size) {
      map.put(i, i * i)
      i += 1
    }
    map
  }

  val stdMap: StdlibMap[Int, Int] = {
    val map = new StdlibMap[Int, Int](initialSize)
    var i = 0
    while (i < size) {
      map.put(i, i * i)
      i += 1
    }
    map
  }

  var randKey: Int = _
  var nonExistingKey: Int = _

  @Setup(Level.Invocation)
  def setup = {
    randKey = random.nextInt(size)
    nonExistingKey = randKey + size
  }

  @Benchmark
  def getRandomSpecialized = specMap.get(randKey)

  @Benchmark
  def getRandomStdlib = stdMap.get(randKey)

  @Benchmark
  def getDirectSpecialized = specMap(randKey)

  @Benchmark
  def getDirectStdlib = stdMap(randKey)

  @Benchmark
  def getNonExistingSpecialized = specMap(nonExistingKey)

  @Benchmark
  def getNonExistingStdlib = stdMap.get(nonExistingKey)

  @Benchmark
  def putAllSpecialized = {
    val m = new HashMap_Int_Int(initialSize = 16)
    var i = 0
    while (i < size) {
      m.put(i, i)
      i += 1
    }
  }

  @Benchmark
  def putAllStdlib = {
    val m = new StdlibMap[Int, Int](initialSize = 16)
    var i = 0
    while (i < size) {
      m.put(i, i)
      i += 1
    }
  }

  @Benchmark
  def foreachMacro(blackhole: Blackhole) =
    specMap foreachMacro ((k, v) => blackhole.consume(k))

  @Benchmark
  def foreachSpecialized(blackhole: Blackhole) =
    specMap foreach ((k, v) => blackhole.consume(k))

  @Benchmark
  def foreachStdlib(blackhole: Blackhole) =
    stdMap foreach (blackhole.consume(_))

  @Benchmark
  def putRemoveReadSpecialized(blackhole: Blackhole) = {
    val map = new HashMap_Int_Int(initialSize = 16)
    var i = 0
    while (i < size) { map.put(i, i); i += 1 }
    i = 0
    while (i < size / 10) { map.remove(i * 10); i += 1 }
    i = 0
    while (i < size) { blackhole.consume(map(i)); i += 1 }
  }

  @Benchmark
  def putRemoveReadStdlib(blackhole: Blackhole) = {
    val map = new StdlibMap[Int, Int](initialSize = 16)
    var i = 0
    while (i < size) { map.put(i, i); i += 1 }
    i = 0
    while (i < size / 10) { map.remove(i * 10); i += 1 }
    i = 0
    while (i < size) { blackhole.consume(map.get(i)); i += 1 }
  }

  @Benchmark
  def mapSpecialized = specMap map ((k, v) => k + 1)

  @Benchmark
  def mapStdlib = stdMap map { case (k, v) => k + 1 }

  @Benchmark
  def flatMapFold =
    specMap.fold (new BufferSeq_Int) { (r, k, v) =>
      var j = 0
      while (j < 5) { r.append(k + j); j += 1 }
      r
    }

  @Benchmark
  def flatMapSpecialized =
    specMap flatMap { (k, v) =>
      val r = new BufferSeq_Int
      var j = 0
      while (j < 5) { r.append(k + j); j += 1 }
      r
    }

  @Benchmark
  def flatMapStdlib =
    stdMap flatMap { case (k, v) =>
      val r = StdlibBuffer[Int]()
      var j = 0
      while (j < 5) { r.append(k + j); j += 1 }
      r
    }

  @Benchmark
  def filterSpecialized = specMap filter ((k, v) => k % 2 == 0)

  @Benchmark
  def filterStdlib = stdMap filter { case (k, v) => k % 2 == 0 }
}

@State(Scope.Thread)
class IntMapRemoveSpecializedBenchmark {

  import Benchmark._

  var map: HashMap_Int_Int = _

  @Setup(Level.Invocation)
  def setup = {
    map = new HashMap_Int_Int
    0 until size foreach (i => map.put(i, i * i))
  }

  @Benchmark
  def benchmark = {
    var i = 0
    while (i < size / 10) { map.remove(i * 10); i += 1 }
  }
}

@State(Scope.Thread)
class IntMapRemoveStdlibBenchmark {

  import Benchmark._

  var map: StdlibMap[Int, Int] = _

  @Setup(Level.Invocation)
  def setup = {
    map = StdlibMap[Int, Int]()
    0 until size foreach (i => map.put(i, i * i))
  }

  @Benchmark
  def benchmark = {
    var i = 0
    while (i < size / 10) { map.remove(i * 10); i += 1 }
  }
}
