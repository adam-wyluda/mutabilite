package benchmark

import offheap.collection._
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

import scala.collection.mutable.{Buffer => StdlibBuffer, OpenHashMap => StdlibMap}

@State(Scope.Thread)
class MixedMapBenchmark {

  import IntBenchmark._

  val specMap: HashMap_Int_Object[String] = {
    val map = new HashMap_Int_Object[String](initialSize)
    var i = 0
    while (i < size) {
      map.put(keys(i), i toString)
      i += 1
    }
    map
  }

  val deboxMap: debox.Map[Int, String] = {
    val map = debox.Map.ofSize[Int, String](size)
    var i = 0
    while (i < size) {
      map.update(keys(i), i toString)
      i += 1
    }
    map
  }

  val stdMap: StdlibMap[Int, Int] = {
    val map = new StdlibMap[Int, Int](initialSize)
    var i = 0
    while (i < size) {
      map.put(keys(i), i * i)
      i += 1
    }
    map
  }

  var randKey: Int = _
  var nonExistingKey: Int = _

  @Setup(Level.Invocation)
  def setup = {
    randKey = keys(random.nextInt(size))
    nonExistingKey = random.nextInt
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
    val m = new HashMap_Int_Object[String](initialSize = initialSize)
    var i = 0
    while (i < size) {
      m.put(keys(i), "x")
      i += 1
    }
  }

  @Benchmark
  def putAllDebox = {
    val m = debox.Map.ofSize[Int, String](size)
    var i = 0
    while (i < size) {
      m.update(keys(i), "x")
      i += 1
    }
  }

  @Benchmark
  def putAllStdlib = {
    val m = new StdlibMap[Int, String](initialSize = initialSize)
    var i = 0
    while (i < size) {
      m.put(keys(i), "x")
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
  def foreachDebox(blackhole: Blackhole) =
    deboxMap foreach ((k, v) => blackhole.consume(v))

  @Benchmark
  def foreachStdlib(blackhole: Blackhole) =
    stdMap foreach (blackhole.consume(_))

  @Benchmark
  def putRemoveReadSpecialized(blackhole: Blackhole) = {
    val map = new HashMap_Int_Object[String](initialSize = initialSize)
    var i = 0
    while (i < size) { map.put(keys(i), "x"); i += 1 }
    i = 0
    while (i < size / 10) { map.remove(keys(i * 10)); i += 1 }
    i = 0
    while (i < size) { blackhole.consume(map.get(i)); i += 1 }
  }

  @Benchmark
  def putRemoveReadDebox(blackhole: Blackhole) = {
    val map = debox.Map.ofSize[Int, String](size)
    var i = 0
    while (i < size) { map.update(keys(i), "x"); i += 1 }
    i = 0
    while (i < size / 10) { map.remove(keys(i * 10)); i += 1 }
    i = 0
    while (i < size) { blackhole.consume(map.get(i)); i += 1 }
  }

  @Benchmark
  def putRemoveReadStdlib(blackhole: Blackhole) = {
    val map = new StdlibMap[Int, String](initialSize = initialSize)
    var i = 0
    while (i < size) { map.put(keys(i), "x"); i += 1 }
    i = 0
    while (i < size / 10) { map.remove(keys(i * 10)); i += 1 }
    i = 0
    while (i < size) { blackhole.consume(map.get(i)); i += 1 }
  }

  @Benchmark
  def mapSpecialized = specMap map ((k, v) => k)

  @Benchmark
  def mapDebox = deboxMap mapToArray ((k, v) => k)

  @Benchmark
  def mapStdlib = stdMap map { case (k, v) => k }
}

@State(Scope.Thread)
class MixedMapRemoveSpecializedBenchmark {

  import IntBenchmark._

  var map: HashMap_Int_Object[String] = _

  @Setup(Level.Invocation)
  def setup = {
    map = new HashMap_Int_Object[String](initialSize = initialSize)
    0 until size foreach (i => map.put(keys(i), i toString))
  }

  @Benchmark
  def benchmark = {
    var i = 0
    while (i < size / 10) { map.remove(keys(i)); i += 1 }
  }
}

@State(Scope.Thread)
class MixedMapRemoveDeboxBenchmark {

  import IntBenchmark._

  var map: debox.Map[Int, String] = _

  @Setup(Level.Invocation)
  def setup = {
    map = debox.Map.ofSize[Int, String](size)
    0 until size foreach (i => map.update(keys(i), i toString))
  }

  @Benchmark
  def benchmark = {
    var i = 0
    while (i < size / 10) { map.remove(keys(i)); i += 1 }
  }
}

@State(Scope.Thread)
class MixedMapRemoveStdlibBenchmark {

  import IntBenchmark._

  var map: StdlibMap[Int, String] = _

  @Setup(Level.Invocation)
  def setup = {
    map = new StdlibMap[Int, String](initialSize = initialSize)
    0 until size foreach (i => map.put(keys(i), i toString))
  }

  @Benchmark
  def benchmark = {
    var i = 0
    while (i < size / 10) { map.remove(keys(i)); i += 1 }
  }
}
