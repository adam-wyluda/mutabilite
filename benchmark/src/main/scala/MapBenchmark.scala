package benchmark

import org.openjdk.jmh.annotations._
import offheap.collection._
import org.openjdk.jmh.infra.Blackhole

import scala.collection.mutable.{OpenHashMap => StdlibMap}

@State(Scope.Thread)
class MapBenchmark {

  import Benchmark._

  val specMap: HashMap_Object_Int[Key] = {
    val map = new HashMap_Object_Int[Key](initialSize)
    var i = 0
    while (i < size) {
      map.put(keys(i), i * i)
      i += 1
    }
    map
  }

  val deboxMap: debox.Map[Key, Int] = {
    val map = debox.Map.ofSize[Key, Int](size)
    var i = 0
    while (i < size) {
      map.update(keys(i), i * i)
      i += 1
    }
    map
  }

  val stdMap: StdlibMap[Key, Int] = {
    val map = new StdlibMap[Key, Int](initialSize)
    var i = 0
    while (i < size) {
      map.put(keys(i), i * i)
      i += 1
    }
    map
  }

  var randKey: Key = _
  var nonExistingKey: Key = _

  @Setup(Level.Invocation)
  def setup = {
    randKey = keys(random.nextInt(size))
    nonExistingKey = Key.generate
  }

  @Benchmark
  def getRandomSpecialized = specMap.get(randKey)

  @Benchmark
  def getRandomDebox = deboxMap.get(randKey)

  @Benchmark
  def getRandomStdlib = stdMap.get(randKey)

  @Benchmark
  def getDirectSpecialized = specMap(randKey)

  @Benchmark
  def getDirectDebox = deboxMap(randKey)

  @Benchmark
  def getDirectStdlib = stdMap(randKey)

  @Benchmark
  def getNonExistingSpecialized = specMap.get(nonExistingKey)

  @Benchmark
  def getNonExistingDebox = deboxMap.get(nonExistingKey)

  @Benchmark
  def getNonExistingStdlib = stdMap.get(nonExistingKey)

  @Benchmark
  def putAllSpecialized = {
    val m = new HashMap_Object_Int[Key](initialSize = initialSize)
    var i = 0
    while (i < size) {
      m.put(keys(i), i)
      i += 1
    }
  }

  @Benchmark
  def putAllDebox = {
    val m = debox.Map.ofSize[Key, Int](size)
    var i = 0
    while (i < size) {
      m.update(keys(i), i)
      i += 1
    }
  }

  @Benchmark
  def putAllStdlib = {
    val m = new StdlibMap[Key, Int](initialSize = initialSize)
    var i = 0
    while (i < size) {
      m.put(keys(i), i)
      i += 1
    }
  }

  @Benchmark
  def foreachSpecialized(blackhole: Blackhole) =
    specMap foreach ((k, v) => blackhole.consume(k))

  @Benchmark
  def foreachDebox(blackhole: Blackhole) =
    deboxMap foreach ((k, v) => blackhole.consume(k))

  @Benchmark
  def foreachStdlib(blackhole: Blackhole) =
    stdMap foreach (blackhole.consume(_))

  @Benchmark
  def putRemoveReadSpecialized(blackhole: Blackhole) = {
    val map = new HashMap_Object_Int[Key](initialSize = initialSize)
    var i = 0
    while (i < size) { map.put(keys(i), i); i += 1 }
    i = 0
    while (i < size / 10) { map.remove(keys(i * 10)); i += 1 }
    i = 0
    while (i < size) { blackhole.consume(map.get(keys(i))); i += 1 }
  }

  @Benchmark
  def putRemoveReadDebox(blackhole: Blackhole) = {
    val map = debox.Map.ofSize[Key, Int](size)
    var i = 0
    while (i < size) { map.update(keys(i), i); i += 1 }
    i = 0
    while (i < size / 10) { map.remove(keys(i * 10)); i += 1 }
    i = 0
    while (i < size) { blackhole.consume(map.get(keys(i))); i += 1 }
  }

  @Benchmark
  def putRemoveReadStdlib(blackhole: Blackhole) = {
    val map = new StdlibMap[Key, Int](initialSize = initialSize)
    var i = 0
    while (i < size) { map.put(keys(i), i); i += 1 }
    i = 0
    while (i < size / 10) { map.remove(keys(i * 10)); i += 1 }
    i = 0
    while (i < size) { blackhole.consume(map.get(keys(i))); i += 1 }
  }
}

@State(Scope.Thread)
class MapRemoveSpecializedBenchmark {

  import Benchmark._

  var map: HashMap_Object_Int[Key] = _

  @Setup(Level.Invocation)
  def setup = {
    map = new HashMap_Object_Int[Key](initialSize = initialSize)
    0 until size foreach (i => map.put(keys(i), i * i))
  }

  @Benchmark
  def benchmark = {
    var i = 0
    while (i < size / 10) { map.remove(keys(i * 10)); i += 1 }
  }
}

@State(Scope.Thread)
class MapRemoveDeboxBenchmark {

  import Benchmark._

  var map: debox.Map[Key, Int] = _

  @Setup(Level.Invocation)
  def setup = {
    map = debox.Map.ofSize[Key, Int](size)
    0 until size foreach (i => map.update(keys(i), i * i))
  }

  @Benchmark
  def benchmark = {
    var i = 0
    while (i < size / 10) { map.remove(keys(i * 10)); i += 1 }
  }
}

@State(Scope.Thread)
class MapRemoveStdlibBenchmark {

  import Benchmark._

  var map: StdlibMap[Key, Int] = _

  @Setup(Level.Invocation)
  def setup = {
    map = new StdlibMap[Key, Int](initialSize = initialSize)
    0 until size foreach (i => map.put(keys(i), i * i))
  }

  @Benchmark
  def benchmark = {
    var i = 0
    while (i < size / 10) { map.remove(keys(i * 10)); i += 1 }
  }
}
