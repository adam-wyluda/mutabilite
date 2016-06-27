package benchmark

import org.openjdk.jmh.annotations._
import offheap.collection._
import HashEq.Implicits._
import org.openjdk.jmh.infra.Blackhole

import scala.collection.mutable.{OpenHashMap => StdlibMap}

@State(Scope.Thread)
class IntMapBenchmark {

  import Benchmark._

  implicit val allocator = scala.offheap.malloc

  def test = {
    val opt = OffheapOpt_Int(20)
  }

  val offheapMap: OffheapHashMap_Int_Int = {
    val map = new OffheapHashMap_Int_Int(initialSize)
    var i = 0
    while (i < size) {
      map.put(i, i * i)
      i += 1
    }
    map
  }

  val specMap: HashMap_Int_Int = {
    val map = new HashMap_Int_Int(initialSize)
    var i = 0
    while (i < size) {
      map.put(i, i * i)
      i += 1
    }
    map
  }

  val genericMap: HashMap[Int, Int] = {
    val map = new HashMap[Int, Int](initialSize)
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
  def getRandomOffheap = offheapMap(randKey)

  @Benchmark
  def getRandomSpecialized = specMap(randKey)

  @Benchmark
  def getRandomGeneric = genericMap(randKey)

  @Benchmark
  def getRandomStdlib = stdMap.get(randKey)

  @Benchmark
  def getNonExistingOffheap = offheapMap(nonExistingKey)

  @Benchmark
  def getNonExistingSpecialized = specMap(nonExistingKey)

  @Benchmark
  def getNonExistingGeneric = genericMap(nonExistingKey)

  @Benchmark
  def getNonExistingStdlib = stdMap.get(nonExistingKey)

  @Benchmark
  def putAllOffheap = {
    val m = new OffheapHashMap_Int_Int(initialSize)
    var i = 0
    while (i < size) {
      m.put(i, i)
      i += 1
    }
  }

  @Benchmark
  def putAllSpecialized = {
    val m = new HashMap_Int_Int(initialSize)
    var i = 0
    while (i < size) {
      m.put(i, i)
      i += 1
    }
  }

  @Benchmark
  def putAllGeneric = {
    val m = new HashMap[Int, Int](initialSize)
    var i = 0
    while (i < size) {
      m.put(i, i)
      i += 1
    }
  }

  @Benchmark
  def putAllStdlib = {
    val m = new StdlibMap[Int, Int](initialSize)
    var i = 0
    while (i < size) {
      m.put(i, i)
      i += 1
    }
  }

  @Benchmark
  def foreachOffheap(blackhole: Blackhole) =
    offheapMap foreach ((k, v) => blackhole.consume(k))

  @Benchmark
  def foreachSpecialized(blackhole: Blackhole) =
    specMap foreach ((k, v) => blackhole.consume(k))

  @Benchmark
  def foreachGeneric(blackhole: Blackhole) =
    genericMap foreachGeneric (blackhole.consume(_))

  @Benchmark
  def foreachStdlib(blackhole: Blackhole) =
    stdMap foreach (blackhole.consume(_))

  @Benchmark
  def putRemoveReadOffheap(blackhole: Blackhole) = {
    val map = new OffheapHashMap_Int_Int(initialSize)
    var i = 0
    while (i < size) { map.put(i, i); i += 1 }
    i = 0
    while (i < size / 10) { map.remove(i * 10); i += 1 }
    i = 0
    while (i < size) { blackhole.consume(map(i)); i += 1 }
  }

  @Benchmark
  def putRemoveReadSpecialized(blackhole: Blackhole) = {
    val map = new HashMap_Int_Int(initialSize)
    var i = 0
    while (i < size) { map.put(i, i); i += 1 }
    i = 0
    while (i < size / 10) { map.remove(i * 10); i += 1 }
    i = 0
    while (i < size) { blackhole.consume(map(i)); i += 1 }
  }

  @Benchmark
  def putRemoveReadGeneric(blackhole: Blackhole) = {
    val map = new HashMap[Int, Int](initialSize)
    var i = 0
    while (i < size) { map.put(i, i); i += 1 }
    i = 0
    while (i < size / 10) { map.remove(i * 10); i += 1 }
    i = 0
    while (i < size) { blackhole.consume(map(i)); i += 1 }
  }

  @Benchmark
  def putRemoveReadStdlib(blackhole: Blackhole) = {
    val map = new StdlibMap[Int, Int](initialSize)
    var i = 0
    while (i < size) { map.put(i, i); i += 1 }
    i = 0
    while (i < size / 10) { map.remove(i * 10); i += 1 }
    i = 0
    while (i < size) { blackhole.consume(map.get(i)); i += 1 }
  }
}

@State(Scope.Thread)
class IntMapRemoveOffheapBenchmark {

  import Benchmark._

  implicit val allocator = scala.offheap.malloc

  var map: OffheapHashMap_Int_Int = _

  @Setup(Level.Invocation)
  def setup = {
    map = new OffheapHashMap_Int_Int
    0 until size foreach (i => map.put(i, i * i))
  }

  @Benchmark
  def benchmark = {
    var i = 0
    while (i < size / 10) { map.remove(i * 10); i += 1 }
  }
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
class IntMapRemoveGenericBenchmark {

  import Benchmark._

  var map: HashMap[Int, Int] = _

  @Setup(Level.Invocation)
  def setup = {
    map = new HashMap[Int, Int]
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
