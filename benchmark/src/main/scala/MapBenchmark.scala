package benchmark

import org.openjdk.jmh.annotations._
import offheap.collection._
import HashEq.Implicits._
import org.openjdk.jmh.infra.Blackhole

import scala.collection.mutable.{OpenHashMap => StdlibMap}

@State(Scope.Thread)
class MapBenchmark {

  import Benchmark._

  val map: HashMap_Object_Int = {
    val map = new HashMap_Object_Int(initialSize)
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

  @Benchmark
  def getRandom = map(randKey)

  @Benchmark
  def getRandomStdlib = stdMap.get(randKey)

  @Benchmark
  def getNonExisting = map(nonExistingKey)

  @Benchmark
  def getNonExistingStdlib = stdMap.get(nonExistingKey)

  @Benchmark
  def putAll = {
    val m = new HashMap_Object_Int(initialSize)
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

  @Benchmark
  def foreach(blackhole: Blackhole) = map foreach (blackhole.consume(_))

  @Benchmark
  def foreachStdlib(blackhole: Blackhole) =
    stdMap foreach (blackhole.consume(_))

  @Benchmark
  def putRemoveRead(blackhole: Blackhole) = {
    val map = new HashMap_Object_Int(initialSize)
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

@State(Scope.Thread)
class MapRemoveBenchmark {

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
