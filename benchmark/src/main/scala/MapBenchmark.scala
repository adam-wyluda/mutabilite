package benchmark

import org.openjdk.jmh.annotations._
import offheap.collection._
import HashEq.Implicits._
import org.openjdk.jmh.infra.Blackhole

import scala.collection.mutable.{OpenHashMap => StdlibMap}

@State(Scope.Thread)
class MapBenchmark {

  import Benchmark._

//  val offheapMap: OffheapHashMap_Object_Int = {
//    val map = new OffheapHashMap_Object_Int(initialSize)
//    var i = 0
//    while (i < size) {
//      map.put(keys(i), i * i)
//      i += 1
//    }
//    map
//  }

  val specMap: HashMap_Object_Int = {
    val map = new HashMap_Object_Int(initialSize)
    var i = 0
    while (i < size) {
      map.put(keys(i), i * i)
      i += 1
    }
    map
  }

  val genericMap: HashMap[Object, Int] = {
    val map = new HashMap[Object, Int](initialSize)
    var i = 0
    while (i < size) {
      map.put(keys(i), i * i)
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

//  @Benchmark
//  def getRandomOffheap = offheapMap(randKey)

  @Benchmark
  def getRandomSpecialized = specMap(randKey)

  @Benchmark
  def getRandomGeneric = genericMap(randKey)

  @Benchmark
  def getRandomStdlib = stdMap.get(randKey)

//  @Benchmark
//  def getNonExistingOffheap = offheapMap(nonExistingKey)

  @Benchmark
  def getNonExistingSpecialized = specMap(nonExistingKey)

  @Benchmark
  def getNonExistingGeneric = genericMap(nonExistingKey)

  @Benchmark
  def getNonExistingStdlib = stdMap.get(nonExistingKey)

//  @Benchmark
//  def putAllOffheap = {
//    val m = new OffheapHashMap_Object_Int(initialSize)
//    var i = 0
//    while (i < size) {
//      m.put(keys(i), i)
//      i += 1
//    }
//  }

  @Benchmark
  def putAllSpecialized = {
    val m = new HashMap_Object_Int(initialSize)
    var i = 0
    while (i < size) {
      m.put(keys(i), i)
      i += 1
    }
  }

  @Benchmark
  def putAllGeneric = {
    val m = new HashMap[Object, Int](initialSize)
    var i = 0
    while (i < size) {
      m.put(keys(i), i)
      i += 1
    }
  }

  @Benchmark
  def putAllStdlib = {
    val m = new StdlibMap[Key, Int](initialSize)
    var i = 0
    while (i < size) {
      m.put(keys(i), i)
      i += 1
    }
  }

//  @Benchmark
//  def foreachOffheap(blackhole: Blackhole) =
//    offheapMap foreach ((k, v) => blackhole.consume(k))

  @Benchmark
  def foreachSpecialized(blackhole: Blackhole) =
    specMap foreach ((k, v) => blackhole.consume(k))

  @Benchmark
  def foreachGeneric(blackhole: Blackhole) =
    genericMap foreachGeneric (blackhole.consume(_))

  @Benchmark
  def foreachStdlib(blackhole: Blackhole) =
    stdMap foreach (blackhole.consume(_))

//  @Benchmark
//  def putRemoveReadOffheap(blackhole: Blackhole) = {
//    val map = new OffheapHashMap_Object_Int(initialSize)
//    var i = 0
//    while (i < size) { map.put(keys(i), i); i += 1 }
//    i = 0
//    while (i < size / 10) { map.remove(keys(i * 10)); i += 1 }
//    i = 0
//    while (i < size) { blackhole.consume(map(keys(i))); i += 1 }
//  }

  @Benchmark
  def putRemoveReadSpecialized(blackhole: Blackhole) = {
    val map = new HashMap_Object_Int(initialSize)
    var i = 0
    while (i < size) { map.put(keys(i), i); i += 1 }
    i = 0
    while (i < size / 10) { map.remove(keys(i * 10)); i += 1 }
    i = 0
    while (i < size) { blackhole.consume(map(keys(i))); i += 1 }
  }

  @Benchmark
  def putRemoveReadGeneric(blackhole: Blackhole) = {
    val map = new HashMap[Object, Int](initialSize)
    var i = 0
    while (i < size) { map.put(keys(i), i); i += 1 }
    i = 0
    while (i < size / 10) { map.remove(keys(i * 10)); i += 1 }
    i = 0
    while (i < size) { blackhole.consume(map(keys(i))); i += 1 }
  }

  @Benchmark
  def putRemoveReadStdlib(blackhole: Blackhole) = {
    val map = new StdlibMap[Key, Int](initialSize)
    var i = 0
    while (i < size) { map.put(keys(i), i); i += 1 }
    i = 0
    while (i < size / 10) { map.remove(keys(i * 10)); i += 1 }
    i = 0
    while (i < size) { blackhole.consume(map.get(keys(i))); i += 1 }
  }
}

//@State(Scope.Thread)
//class MapRemoveOffheapBenchmark {
//
//  import Benchmark._
//
//  var map: OffheapHashMap_Object_Int = _
//
//  @Setup(Level.Invocation)
//  def setup = {
//    map = new OffheapHashMap_Object_Int
//    0 until size foreach (i => map.put(keys(i), i * i))
//  }
//
//  @Benchmark
//  def benchmark = {
//    var i = 0
//    while (i < size / 10) { map.remove(keys(i * 10)); i += 1 }
//  }
//}

@State(Scope.Thread)
class MapRemoveSpecializedBenchmark {

  import Benchmark._

  var map: HashMap_Object_Int = _

  @Setup(Level.Invocation)
  def setup = {
    map = new HashMap_Object_Int
    0 until size foreach (i => map.put(keys(i), i * i))
  }

  @Benchmark
  def benchmark = {
    var i = 0
    while (i < size / 10) { map.remove(keys(i * 10)); i += 1 }
  }
}

@State(Scope.Thread)
class MapRemoveGenericBenchmark {

  import Benchmark._

  var map: HashMap[Object, Int] = _

  @Setup(Level.Invocation)
  def setup = {
    map = new HashMap[Object, Int]
    0 until size foreach (i => map.put(keys(i), i * i))
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
    map = StdlibMap[Key, Int]()
    0 until size foreach (i => map.put(keys(i), i * i))
  }

  @Benchmark
  def benchmark = {
    var i = 0
    while (i < size / 10) { map.remove(keys(i * 10)); i += 1 }
  }
}
