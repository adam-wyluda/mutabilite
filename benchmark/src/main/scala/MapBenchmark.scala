package benchmark

import org.openjdk.jmh.annotations._
import offheap.collection._
import org.openjdk.jmh.infra.Blackhole

import scala.util.Random
import scala.collection.mutable.{OpenHashMap => StdlibMap}

@State(Scope.Thread)
class MapBenchmark {

  val keys: Array[String] = {
    val keys = new Array[String](10000)
    0 until 10000 foreach (i => keys(i) = i toString)
    keys
  }

  val map: HashMap[String, Int] = {
    val map = new HashMap[String, Int]()
    0 until 1000 foreach (i => map.put(keys(i), i * i))
    map
  }

  val stdMap: StdlibMap[String, Int] = {
    val map = StdlibMap[String, Int]()
    0 until 1000 foreach (i => map.put(keys(i), i * i))
    map
  }

  val random = Random
  var randKey: String = _
  var nonexistingKey: String = _

  @Setup(Level.Invocation)
  def setup = {
    randKey = keys(random.nextInt(10000))
    nonexistingKey = (random.nextInt(10000) + 10000) toString
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
    while (i < 10000) {
      m.put(keys(i), i)
      i += 1
    }
  }

  @Benchmark
  def putStdlib = {
    val m = StdlibMap[String, Int]()
    var i = 0
    while (i < 10000) {
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

@State(Scope.Thread)
trait MapCollidingKeysBenchmark {

  class Key(val value: Int) {
    override def equals(other: Any) = other.asInstanceOf[Key].value == this.value
    override def hashCode = value % 100
  }

  val keys = {
    val keys = new Array[Key](10000)
    0 until keys.size foreach (i => keys(i) = new Key(i))
    keys
  }

  @Benchmark
  def put = {
    val map = new HashMap[Key, Int]
    val size = keys.size
    var i = 0
    while (i < size) {
      map.put(keys(i), i)
      i += 1
    }
  }

  @Benchmark
  def putStdlibLinked = {
    val map = StdlibMap[Key, Int]()
    val size = keys.size
    var i = 0
    while (i < size) {
      map.put(keys(i), i)
      i += 1
    }
  }

  @Benchmark
  def putStdlibOpenAddressing = {
    val map = collection.mutable.OpenHashMap[Key, Int]()
    val size = keys.size
    var i = 0
    while (i < size) {
      map.put(keys(i), i)
      i += 1
    }
  }
}
