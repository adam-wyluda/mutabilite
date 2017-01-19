package benchmark

import java.util.function.BiConsumer

import org.openjdk.jmh.annotations._
import mutabilite._
import org.openjdk.jmh.infra.Blackhole

import scala.collection.mutable.{Buffer => StdlibBuffer, OpenHashMap => StdlibMap}
import java.util.{HashMap => JavaMap}

@State(Scope.Thread)
class IntMapBenchmark {

  import IntBenchmark._

  val specMap: Map_Int_Int = {
    val map = new Map_Int_Int(initialSize)
    var i = 0
    while (i < size) {
      map.put(keys(i), i * i)
      i += 1
    }
    map
  }

//  val deboxMap: debox.Map[Int, Int] = {
//    val map = debox.Map.ofSize[Int, Int](size)
//    var i = 0
//    while (i < size) {
//      map.update(keys(i), i * i)
//      i += 1
//    }
//    map
//  }

  val stdMap: StdlibMap[Int, Int] = {
    val map = new StdlibMap[Int, Int](initialSize)
    var i = 0
    while (i < size) {
      map.put(keys(i), i * i)
      i += 1
    }
    map
  }

  val javaMap: JavaMap[Int, Int] = {
    val map = new JavaMap[Int, Int](initialSize)
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
  def getDirectSpecialized = specMap(randKey)

//  @Benchmark
//  def getDirectDebox = deboxMap(randKey)

  @Benchmark
  def getDirectStdlib = stdMap(randKey)

  @Benchmark
  def getDirectJavalib = javaMap.get(randKey)

  @Benchmark
  def getNonExistingSpecialized = specMap.get(nonExistingKey)

//  @Benchmark
//  def getNonExistingDebox = deboxMap.get(nonExistingKey)

  @Benchmark
  def getNonExistingStdlib = stdMap.get(nonExistingKey)

  @Benchmark
  def getNonExistingJavalib = javaMap.get(nonExistingKey)

  @Benchmark
  def putAllSpecialized = {
    val m = new Map_Int_Int(initialSize = initialSize)
    var i = 0
    while (i < size) {
      m.put(keys(i), i)
      i += 1
    }
  }

//  @Benchmark
//  def putAllDebox = {
//    val m = debox.Map.ofSize[Int, Int](size)
//    var i = 0
//    while (i < size) {
//      m.update(keys(i), i)
//      i += 1
//    }
//  }

  @Benchmark
  def putAllStdlib = {
    val m = new StdlibMap[Int, Int](initialSize = initialSize)
    var i = 0
    while (i < size) {
      m.put(keys(i), i)
      i += 1
    }
  }

  @Benchmark
  def putAllJavalib = {
    val m = new JavaMap[Int, Int](initialSize)
    var i = 0
    while (i < size) {
      m.put(keys(i), i)
      i += 1
    }
  }

  @Benchmark
  def foreachSpecialized(blackhole: Blackhole) =
    specMap foreach ((k, v) => blackhole.consume(k))

//  @Benchmark
//  def foreachDebox(blackhole: Blackhole) =
//    deboxMap foreach ((k, v) => blackhole.consume(k))

  @Benchmark
  def foreachStdlib(blackhole: Blackhole) =
    stdMap foreach (blackhole.consume(_))

  @Benchmark
  def foreachJavalib(blackhole: Blackhole) =
    javaMap forEach (new BiConsumer[Int, Int] {
      def accept(t: Int, x: Int): Unit = blackhole.consume(t)
    })

  @Benchmark
  def putRemoveReadSpecialized(blackhole: Blackhole) = {
    val map = new Map_Int_Int(initialSize = initialSize)
    var i = 0
    while (i < size) { map.put(keys(i), i); i += 1 }
    i = 0
    while (i < size / 10) { map.remove(keys(i * 10)); i += 1 }
    i = 0
    while (i < size) { blackhole.consume(map.get(i)); i += 1 }
  }

//  @Benchmark
//  def putRemoveReadDebox(blackhole: Blackhole) = {
//    val map = debox.Map.ofSize[Int, Int](size)
//    var i = 0
//    while (i < size) { map.update(keys(i), i); i += 1 }
//    i = 0
//    while (i < size / 10) { map.remove(keys(i * 10)); i += 1 }
//    i = 0
//    while (i < size) { blackhole.consume(map.get(i)); i += 1 }
//  }

  @Benchmark
  def putRemoveReadStdlib(blackhole: Blackhole) = {
    val map = new StdlibMap[Int, Int](initialSize = initialSize)
    var i = 0
    while (i < size) { map.put(keys(i), i); i += 1 }
    i = 0
    while (i < size / 10) { map.remove(keys(i * 10)); i += 1 }
    i = 0
    while (i < size) { blackhole.consume(map.get(i)); i += 1 }
  }

  @Benchmark
  def putRemoveReadJavalib(blackhole: Blackhole) = {
    val map = new JavaMap[Int, Int](initialSize)
    var i = 0
    while (i < size) { map.put(keys(i), i); i += 1 }
    i = 0
    while (i < size / 10) { map.remove(keys(i * 10)); i += 1 }
    i = 0
    while (i < size) { blackhole.consume(map.get(i)); i += 1 }
  }

  @Benchmark
  def mapSpecialized = specMap map ((k, v) => k)

//  @Benchmark
//  def mapDebox = deboxMap mapToArray ((k, v) => k)

  @Benchmark
  def mapStdlib = stdMap map { case (k, v) => k }
}

@State(Scope.Thread)
class IntMapRemoveSpecializedBenchmark {

  import IntBenchmark._

  var map: Map_Int_Int = _

  @Setup(Level.Invocation)
  def setup = {
    map = new Map_Int_Int(initialSize = initialSize)
    0 until size foreach (i => map.put(keys(i), i))
  }

  @Benchmark
  def benchmark = {
    var i = 0
    while (i < size / 10) { map.remove(keys(i * 10)); i += 1 }
  }
}

//@State(Scope.Thread)
//class IntMapRemoveDeboxBenchmark {
//
//  import IntBenchmark._
//
//  var map: debox.Map[Int, Int] = _
//
//  @Setup(Level.Invocation)
//  def setup = {
//    map = debox.Map.ofSize[Int, Int](size)
//    0 until size foreach (i => map.update(keys(i), i))
//  }
//
//  @Benchmark
//  def benchmark = {
//    var i = 0
//    while (i < size / 10) { map.remove(keys(i * 10)); i += 1 }
//  }
//}

@State(Scope.Thread)
class IntMapRemoveStdlibBenchmark {

  import IntBenchmark._

  var map: StdlibMap[Int, Int] = _

  @Setup(Level.Invocation)
  def setup = {
    map = new StdlibMap[Int, Int](initialSize = initialSize)
    0 until size foreach (i => map.put(keys(i), i))
  }

  @Benchmark
  def benchmark = {
    var i = 0
    while (i < size / 10) { map.remove(keys(i * 10)); i += 1 }
  }
}

@State(Scope.Thread)
class IntMapRemoveJavalibBenchmark {

  import IntBenchmark._

  var map: JavaMap[Int, Int] = _

  @Setup(Level.Invocation)
  def setup = {
    map = new JavaMap[Int, Int](initialSize)
    0 until size foreach (i => map.put(keys(i), i))
  }

  @Benchmark
  def benchmark = {
    var i = 0
    while (i < size / 10) { map.remove(keys(i * 10)); i += 1 }
  }
}
