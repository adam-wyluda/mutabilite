package benchmark

import org.openjdk.jmh.annotations._
import offheap.collection._
import HashEq.Implicits._
import org.openjdk.jmh.infra.Blackhole

import scala.offheap.{Pool, Region}
import scala.collection.mutable.{OpenHashMap => StdlibMap}

@State(Scope.Thread)
class IntMapBenchmark {

  import Benchmark._

  implicit val props = Region.Props(Pool(pageSize = 4 * 1024 * 1024, chunkSize = 16 * 1024 * 1024))
  val malloc = scala.offheap.malloc

  val offheapMap: OffheapHashMap_Int_Int = {
    implicit val alloc = malloc
    val map = OffheapMap_Int_Int.create(initialSize)
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

  var freedMap: OffheapHashMap_Int_Int = _

  var randKey: Int = _
  var nonExistingKey: Int = _

  var region: Region = _

  @Setup(Level.Invocation)
  def setup = {
    randKey = random.nextInt(size)
    nonExistingKey = randKey + size
    freedMap = OffheapHashMap_Int_Int.empty
    region = Region.open
  }

  @TearDown(Level.Invocation)
  def tearDown = {
    if (freedMap.nonEmpty) freedMap.free(malloc)
    region.close
  }

  @Benchmark
  def getRandomOffheap = {
    implicit val alloc = region
    offheapMap(randKey)
  }

  @Benchmark
  def getRandomSpecialized = specMap(randKey)

  @Benchmark
  def getRandomGeneric = genericMap(randKey)

  @Benchmark
  def getRandomStdlib = stdMap.get(randKey)

  @Benchmark
  def getNonExistingOffheap = {
    implicit val alloc = region
    offheapMap(nonExistingKey)
  }

  @Benchmark
  def getNonExistingSpecialized = specMap(nonExistingKey)

  @Benchmark
  def getNonExistingGeneric = genericMap(nonExistingKey)

  @Benchmark
  def getNonExistingStdlib = stdMap.get(nonExistingKey)

  @Benchmark
  def putAllOffheap = {
    implicit val alloc = malloc
    freedMap = OffheapMap_Int_Int.create(initialSize = 16)
    var i = 0
    while (i < size) {
      freedMap.put(i, i)
      i += 1
    }
  }

  @Benchmark
  def putAllRegion = {
    implicit val alloc = region
    val s = OffheapMap_Int_Int.create(initialSize = 16)
    var i = 0
    while (i < size) {
      s.put(i, i)
      i += 1
    }
  }

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
  def putAllGeneric = {
    val m = new HashMap[Int, Int](initialSize = 16)
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
    implicit val alloc = malloc
    freedMap = OffheapMap_Int_Int.create(initialSize = 16)
    var i = 0
    while (i < size) { freedMap.put(i, i); i += 1 }
    i = 0
    while (i < size / 10) { freedMap.remove(i * 10); i += 1 }
    i = 0
    while (i < size) { blackhole.consume(freedMap(i)); i += 1 }
  }

  @Benchmark
  def putRemoveReadRegion(blackhole: Blackhole) = {
    implicit val alloc = region
    val s = OffheapMap_Int_Int.create(initialSize = 16)
    var i = 0
    while (i < size) { s.put(i, i); i += 1 }
    i = 0
    while (i < size / 10) { s.remove(i * 10); i += 1 }
    i = 0
    while (i < size) { blackhole.consume(s(i)); i += 1 }
  }

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
  def putRemoveReadGeneric(blackhole: Blackhole) = {
    val map = new HashMap[Int, Int](initialSize = 16)
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
}

@State(Scope.Thread)
class IntMapRemoveOffheapBenchmark {

  import Benchmark._

  implicit val allocator = scala.offheap.malloc

  var map: OffheapHashMap_Int_Int = _

  @Setup(Level.Invocation)
  def setup = {
    map = OffheapMap_Int_Int.create(initialSize)
    0 until size foreach (i => map.put(i, i * i))
  }

  @Setup(Level.Invocation)
  def tearDown = map.free

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
