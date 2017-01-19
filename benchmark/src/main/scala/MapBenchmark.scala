package benchmark

import java.util.function.BiConsumer

import org.openjdk.jmh.annotations._
import mutabilite._
import org.openjdk.jmh.infra.Blackhole

import scala.collection.mutable.{OpenHashMap => StdlibMap}
import java.util.{HashMap => JavaMap}

@State(Scope.Thread)
class MapBenchmark {

  import Benchmark._

  val specMap: Map_Object_Object[Key, Key] = {
    val map = new Map_Object_Object[Key, Key](initialSize)
    var i = 0
    while (i < size) {
      map.put(keys(i), keys(i))
      i += 1
    }
    map
  }

//  val deboxMap: debox.Map[Key, Key] = {
//    val map = debox.Map.ofSize[Key, Key](size)
//    var i = 0
//    while (i < size) {
//      map.update(keys(i), keys(i))
//      i += 1
//    }
//    map
//  }

  val stdMap: StdlibMap[Key, Key] = {
    val map = new StdlibMap[Key, Key](initialSize)
    var i = 0
    while (i < size) {
      map.put(keys(i), keys(i))
      i += 1
    }
    map
  }

  val javaMap: JavaMap[Key, Key] = {
    val map = new JavaMap[Key, Key](initialSize)
    var i = 0
    while (i < size) {
      map.put(keys(i), keys(i))
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
    val m = new Map_Object_Object[Key, Key](initialSize = initialSize)
    var i = 0
    while (i < size) {
      m.put(keys(i), keys(i))
      i += 1
    }
  }

//  @Benchmark
//  def putAllDebox = {
//    val m = debox.Map.ofSize[Key, Key](size)
//    var i = 0
//    while (i < size) {
//      m.update(keys(i), keys(i))
//      i += 1
//    }
//  }

  @Benchmark
  def putAllStdlib = {
    val m = new StdlibMap[Key, Key](initialSize = initialSize)
    var i = 0
    while (i < size) {
      m.put(keys(i), keys(i))
      i += 1
    }
  }

  @Benchmark
  def putAllJavalib = {
    val m = new JavaMap[Key, Key](initialSize)
    var i = 0
    while (i < size) {
      m.put(keys(i), keys(i))
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
    javaMap forEach (new BiConsumer[Key, Key] {
          def accept(t: Key, x: Key): Unit = blackhole.consume(t)
        })

  @Benchmark
  def putRemoveReadSpecialized(blackhole: Blackhole) = {
    val map = new Map_Object_Object[Key, Key](initialSize = initialSize)
    var i = 0
    while (i < size) { map.put(keys(i), keys(i)); i += 1 }
    i = 0
    while (i < size / 10) { map.remove(keys(i * 10)); i += 1 }
    i = 0
    while (i < size) { blackhole.consume(map.get(keys(i))); i += 1 }
  }

//  @Benchmark
//  def putRemoveReadDebox(blackhole: Blackhole) = {
//    val map = debox.Map.ofSize[Key, Key](size)
//    var i = 0
//    while (i < size) { map.update(keys(i), keys(i)); i += 1 }
//    i = 0
//    while (i < size / 10) { map.remove(keys(i * 10)); i += 1 }
//    i = 0
//    while (i < size) { blackhole.consume(map.get(keys(i))); i += 1 }
//  }

  @Benchmark
  def putRemoveReadStdlib(blackhole: Blackhole) = {
    val map = new StdlibMap[Key, Key](initialSize = initialSize)
    var i = 0
    while (i < size) { map.put(keys(i), keys(i)); i += 1 }
    i = 0
    while (i < size / 10) { map.remove(keys(i * 10)); i += 1 }
    i = 0
    while (i < size) { blackhole.consume(map.get(keys(i))); i += 1 }
  }

  @Benchmark
  def putRemoveReadJavalib(blackhole: Blackhole) = {
    val map = new JavaMap[Key, Key](initialSize)
    var i = 0
    while (i < size) { map.put(keys(i), keys(i)); i += 1 }
    i = 0
    while (i < size / 10) { map.remove(keys(i * 10)); i += 1 }
    i = 0
    while (i < size) { blackhole.consume(map.get(i)); i += 1 }
  }
}

@State(Scope.Thread)
class MapRemoveSpecializedBenchmark {

  import Benchmark._

  var map: Map_Object_Object[Key, Key] = _

  @Setup(Level.Invocation)
  def setup = {
    map = new Map_Object_Object[Key, Key](initialSize = initialSize)
    0 until size foreach (i => map.put(keys(i), keys(i)))
  }

  @Benchmark
  def benchmark = {
    var i = 0
    while (i < size / 10) { map.remove(keys(i * 10)); i += 1 }
  }
}

//@State(Scope.Thread)
//class MapRemoveDeboxBenchmark {
//
//  import Benchmark._
//
//  var map: debox.Map[Key, Key] = _
//
//  @Setup(Level.Invocation)
//  def setup = {
//    map = debox.Map.ofSize[Key, Key](size)
//    0 until size foreach (i => map.update(keys(i), keys(i)))
//  }
//
//  @Benchmark
//  def benchmark = {
//    var i = 0
//    while (i < size / 10) { map.remove(keys(i * 10)); i += 1 }
//  }
//}

@State(Scope.Thread)
class MapRemoveStdlibBenchmark {

  import Benchmark._

  var map: StdlibMap[Key, Key] = _

  @Setup(Level.Invocation)
  def setup = {
    map = new StdlibMap[Key, Key](initialSize = initialSize)
    0 until size foreach (i => map.put(keys(i), keys(i)))
  }

  @Benchmark
  def benchmark = {
    var i = 0
    while (i < size / 10) { map.remove(keys(i * 10)); i += 1 }
  }
}

@State(Scope.Thread)
class MapRemoveJavalibBenchmark {

  import Benchmark._

  var map: JavaMap[Key, Key] = _

  @Setup(Level.Invocation)
  def setup = {
    map = new JavaMap[Key, Key](initialSize)
    0 until size foreach (i => map.put(keys(i), keys(i)))
  }

  @Benchmark
  def benchmark = {
    var i = 0
    while (i < size / 10) { map.remove(keys(i * 10)); i += 1 }
  }
}
