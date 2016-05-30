package benchmark

import org.openjdk.jmh.annotations._
import offheap.collection._
import org.openjdk.jmh.infra.Blackhole

import scala.util.Random
import scala.collection.mutable.{HashMap => StdlibMap}

@State(Scope.Thread)
class MapBenchmark {

  val map: Map[Int, Int] = {
    val map = new Map[Int, Int]()
    1 to 10000 foreach (i => map.put(i, i * i))
    map
  }

  val stdMap: StdlibMap[Int, Int] = {
    val map = StdlibMap[Int, Int]()
    1 to 10000 foreach (i => map.put(i, i * i))
    map
  }

  val random = Random

  @Benchmark
  def get(blackhole: Blackhole) = blackhole.consume(map(random.nextInt(20000)))

  @Benchmark
  def getStdlib(blackhole: Blackhole) =
    blackhole.consume(stdMap.get(random.nextInt(20000)))

  @Benchmark
  def put = {
    val m = new Map[Int, Int]
    var i = 0
    while (i < 1000) {
      m.put(i, i)
      i += 1
    }
  }

  @Benchmark
  def putStdlib = {
    val m = StdlibMap[Int, Int]()
    var i = 0
    while (i < 1000) {
      m.put(i, i)
      i += 1
    }
  }

  @Benchmark
  def foreach(blackhole: Blackhole) = map foreach (blackhole.consume(_))

  @Benchmark
  def foreachStdlib(blackhole: Blackhole) =
    stdMap foreach (blackhole.consume(_))
}

@State(Scope.Thread)
class MapRemoveBenchmark {

  var map: Map[Int, Int] = _

  @Setup(Level.Invocation)
  def setup = {
    map = new Map[Int, Int]
    1 to 1000 foreach (i => map.put(i, i * i))
  }

  @Benchmark
  def benchmark = {
    var i = 0
    while (i < 1000) { map.remove(i); i += 1 }
  }
}

@State(Scope.Thread)
class MapRemoveStdlibBenchmark {

  var map: StdlibMap[Int, Int] = _

  @Setup(Level.Invocation)
  def setup = {
    map = StdlibMap[Int, Int]()
    1 to 1000 foreach (i => map.put(i, i * i))
  }

  @Benchmark
  def benchmark = {
    var i = 0
    while (i < 1000) { map.remove(i); i += 1 }
  }
}
