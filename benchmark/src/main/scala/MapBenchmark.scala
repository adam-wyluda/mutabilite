package benchmark

import org.openjdk.jmh.annotations._
import offheap.collection._
import org.openjdk.jmh.infra.Blackhole

import scala.util.Random
import scala.collection.mutable.{OpenHashMap => StdlibMap}

@State(Scope.Thread)
class MapBenchmark {

  val mapSize = 100000
  val random = Random

  val keys: Array[String] = {
    val keys = new Array[String](mapSize)
    0 until mapSize foreach (keys(_) = random.nextString(10))
    keys
  }

  val map: HashMap[String, Int] = {
    val map = new HashMap[String, Int]()
    0 until mapSize foreach (i => map.put(keys(i), i * i))
    map
  }

  val stdMap: StdlibMap[String, Int] = {
    val map = StdlibMap[String, Int]()
    0 until mapSize foreach (i => map.put(keys(i), i * i))
    map
  }

  var randKey: String = _
  var nonexistingKey: String = _

  @Setup(Level.Invocation)
  def setup = {
    randKey = keys(random.nextInt(mapSize))
    nonexistingKey = random.nextString(11)
  }

  @Benchmark
  def get = map(randKey)

  @Benchmark
  def getStdlib = stdMap.get(randKey)

  @Benchmark
  def getNonExisting = map(nonexistingKey)

  @Benchmark
  def getNonExistingStdlib = stdMap.get(nonexistingKey)

  @Benchmark
  def put = {
    val m = new HashMap[String, Int]
    var i = 0
    while (i < mapSize) {
      m.put(keys(i), i)
      i += 1
    }
  }

  @Benchmark
  def putStdlib = {
    val m = StdlibMap[String, Int]()
    var i = 0
    while (i < mapSize) {
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
    val map = new HashMap[String, Int]()
    var i = 0
    while (i < 10000) { map.put(keys(i), i); i += 1 }
    i = 0
    while (i < 1000) { map.remove(keys(i * 10)); i += 1 }
    i = 0
    while (i < 10000) { blackhole.consume(map(keys(i))); i += 1 }
  }

  @Benchmark
  def putRemoveReadStdlib(blackhole: Blackhole) = {
    val map = new StdlibMap[String, Int]()
    var i = 0
    while (i < 10000) { map.put(keys(i), i); i += 1 }
    i = 0
    while (i < 1000) { map.remove(keys(i * 10)); i += 1 }
    i = 0
    while (i < 10000) { blackhole.consume(map.get(keys(i))); i += 1 }
  }
}

@State(Scope.Thread)
class MapRemoveBenchmark {

  var map: HashMap[String, Int] = _
  val keys: Array[String] = {
    val keys = new Array[String](1000)
    0 until 1000 foreach (i => keys(i) = i toString)
    keys
  }

  @Setup(Level.Invocation)
  def setup = {
    map = new HashMap[String, Int]
    0 until 1000 foreach (i => map.put(keys(i), i * i))
  }

  @Benchmark
  def benchmark = {
    var i = 0
    while (i < 1000) { map.remove(keys(i)); i += 1 }
  }
}

@State(Scope.Thread)
class MapRemoveStdlibBenchmark {

  var map: StdlibMap[String, Int] = _
  val keys: Array[String] = {
    val keys = new Array[String](1000)
    0 until 1000 foreach (i => keys(i) = i toString)
    keys
  }

  @Setup(Level.Invocation)
  def setup = {
    map = StdlibMap[String, Int]()
    0 until 1000 foreach (i => map.put(keys(i), i * i))
  }

  @Benchmark
  def benchmark = {
    var i = 0
    while (i < 1000) { map.remove(keys(i)); i += 1 }
  }
}
