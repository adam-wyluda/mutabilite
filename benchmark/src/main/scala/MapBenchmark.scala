package benchmark

import org.openjdk.jmh.annotations._
import offheap.collection._
import org.openjdk.jmh.infra.Blackhole

import scala.util.Random
import scala.collection.mutable.{OpenHashMap => StdlibMap}

object MapBenchmark {

  val mapSize = 1000000
  val random = Random

  class Key(val x: Int, val y: Int, val z: Int) {
    override def equals(that: Any) = {
      val t = that.asInstanceOf[Key]
      x == t.x && y == t.y && z == t.z
    }
    override def hashCode = (x * 31 + y) * 31 + z
  }

  object Key {
    @inline
    def generate = new Key(random.nextInt(), random.nextInt(), random.nextInt())
  }

  val keys: Array[Key] = {
    val keys = new Array[Key](mapSize)
    0 until mapSize foreach (keys(_) = Key.generate)
    keys
  }

  val initialMapSize = {
    var size = 1
    while (size < mapSize) size *= 2
    size
  }
}

@State(Scope.Thread)
class MapBenchmark {

  import MapBenchmark._

  val map: HashMap[Key, Int] = {
    val map = new HashMap[Key, Int](initialSize = initialMapSize)
    0 until mapSize foreach (i => map.put(keys(i), i * i))
    map
  }

  val stdMap: StdlibMap[Key, Int] = {
    val map = new StdlibMap[Key, Int](initialSize = initialMapSize)
    0 until mapSize foreach (i => map.put(keys(i), i * i))
    map
  }

  var randKey: Key = _
  var nonExistingKey: Key = _

  @Setup(Level.Invocation)
  def setup = {
    randKey = keys(random.nextInt(mapSize))
    nonExistingKey = Key.generate
  }

  @Benchmark
  def get = map(randKey)

  @Benchmark
  def getStdlib = stdMap.get(randKey)

  @Benchmark
  def getNonExisting = map(nonExistingKey)

  @Benchmark
  def getNonExistingStdlib = stdMap.get(nonExistingKey)

  @Benchmark
  def put = {
    val m = new HashMap[Key, Int](initialSize = initialMapSize)
    var i = 0
    while (i < mapSize) {
      m.put(keys(i), i)
      i += 1
    }
  }

  @Benchmark
  def putStdlib = {
    val m = new StdlibMap[Key, Int](initialSize = initialMapSize)
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
    val map = new HashMap[Key, Int](initialSize = initialMapSize)
    var i = 0
    while (i < mapSize) { map.put(keys(i), i); i += 1 }
    i = 0
    while (i < mapSize / 10) { map.remove(keys(i * 10)); i += 1 }
    i = 0
    while (i < mapSize) { blackhole.consume(map(keys(i))); i += 1 }
  }

  @Benchmark
  def putRemoveReadStdlib(blackhole: Blackhole) = {
    val map = new StdlibMap[Key, Int](initialSize = initialMapSize)
    var i = 0
    while (i < mapSize) { map.put(keys(i), i); i += 1 }
    i = 0
    while (i < mapSize / 10) { map.remove(keys(i * 10)); i += 1 }
    i = 0
    while (i < mapSize) { blackhole.consume(map.get(keys(i))); i += 1 }
  }
}

@State(Scope.Thread)
class MapRemoveBenchmark {

  import MapBenchmark._

  var map: HashMap[Key, Int] = _

  @Setup(Level.Invocation)
  def setup = {
    map = new HashMap[Key, Int]
    0 until mapSize foreach (i => map.put(keys(i), i * i))
  }

  @Benchmark
  def benchmark = {
    var i = 0
    while (i < mapSize / 10) { map.remove(keys(i * 10)); i += 1 }
  }
}

@State(Scope.Thread)
class MapRemoveStdlibBenchmark {

  import MapBenchmark._

  var map: StdlibMap[Key, Int] = _

  @Setup(Level.Invocation)
  def setup = {
    map = StdlibMap[Key, Int]()
    0 until mapSize foreach (i => map.put(keys(i), i * i))
  }

  @Benchmark
  def benchmark = {
    var i = 0
    while (i < mapSize / 10) { map.remove(keys(i * 10)); i += 1 }
  }
}
