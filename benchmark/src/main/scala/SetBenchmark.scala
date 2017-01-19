package benchmark

import java.util.function.Consumer

import org.openjdk.jmh.annotations._
import mutabilite._
import org.openjdk.jmh.infra.Blackhole

import scala.collection.mutable.{HashSet => StdlibSet}
import java.util.{HashSet => JavaSet}

@State(Scope.Thread)
class SetBenchmark {

  import Benchmark._

  val specSet: Set_Object[Key] = {
    val set = new Set_Object[Key](initialSize)
    var i = 0
    while (i < size) {
      set.add(keys(i))
      i += 1
    }
    set
  }

//  val deboxSet: debox.Set[Key] = {
//    val set = debox.Set.ofSize[Key](size)
//    var i = 0
//    while (i < size) {
//      set.add(keys(i))
//      i += 1
//    }
//    set
//  }

  val stdSet: StdlibSet[Key] = {
    val set = new StdlibSet[Key] { override val initialSize = Benchmark.initialSize }
    var i = 0
    while (i < size) {
      set.add(keys(i))
      i += 1
    }
    set
  }

  val javaSet: JavaSet[Key] = {
    val set = new JavaSet[Key](initialSize)
    var i = 0
    while (i < size) {
      set.add(keys(i))
      i += 1
    }
    set
  }

  var randKey: Key = _
  var nonExistingKey: Key = _

  @Setup(Level.Invocation)
  def setup = {
    randKey = keys(random.nextInt(size))
    nonExistingKey = Key.generate
  }

  @Benchmark
  def containsExistingSpecialized = specSet(randKey)

//  @Benchmark
//  def containsExistingDebox = deboxSet(randKey)

  @Benchmark
  def containsExistingStdlib = stdSet(randKey)

  @Benchmark
  def containsExistingJavalib = javaSet.contains(randKey)

  @Benchmark
  def containsNonExistingSpecialized = specSet(nonExistingKey)

//  @Benchmark
//  def containsNonExistingDebox = deboxSet(nonExistingKey)

  @Benchmark
  def containsNonExistingStdlib = stdSet(nonExistingKey)

  @Benchmark
  def containsNonExistingJavalib = javaSet.contains(nonExistingKey)

  @Benchmark
  def addSpecialized = {
    val s = new Set_Object[Key](initialSize = initialSize)
    var i = 0
    while (i < size) {
      s.add(keys(i))
      i += 1
    }
  }

//  @Benchmark
//  def addDebox = {
//    val s = debox.Set.ofSize[Key](size)
//    var i = 0
//    while (i < size) {
//      s.add(keys(i))
//      i += 1
//    }
//  }

  @Benchmark
  def addStdlib = {
    val s = new StdlibSet[Key] { override val initialSize = Benchmark.initialSize }
    var i = 0
    while (i < size) {
      s.add(keys(i))
      i += 1
    }
  }

  @Benchmark
  def addJavalib = {
    val s = new JavaSet[Key](initialSize)
    var i = 0
    while (i < size) {
      s.add(keys(i))
      i += 1
    }
  }

  @Benchmark
  def foreachSpecialized(blackhole: Blackhole) =
    specSet foreach (blackhole.consume(_))

//  @Benchmark
//  def foreachDebox(blackhole: Blackhole) =
//    deboxSet foreach (blackhole.consume(_))

  @Benchmark
  def foreachStdlib(blackhole: Blackhole) =
    stdSet foreach (blackhole.consume(_))

  @Benchmark
  def foreachJavalib(blackhole: Blackhole) =
    javaSet.forEach(new Consumer[Key] {
      def accept(t: Key): Unit = blackhole.consume(t)
    })
}

@State(Scope.Thread)
class SetRemoveSpecializedBenchmark {

  import Benchmark._

  var set: Set_Object[Key] = _

  @Setup(Level.Invocation)
  def setup = {
    set = new Set_Object[Key](initialSize)
    var i = 0
    while (i < size) {
      set.add(keys(i))
      i += 1
    }
  }

  @Benchmark
  def benchmark = {
    var i = 0
    while (i < size / 10) { set.remove(keys(i * 10)); i += 1 }
  }
}

//@State(Scope.Thread)
//class SetRemoveDeboxBenchmark {
//
//  import Benchmark._
//
//  var set: debox.Set[Key] = _
//
//  @Setup(Level.Invocation)
//  def setup = {
//    set = debox.Set.ofSize[Key](size)
//    var i = 0
//    while (i < size) {
//      set.add(keys(i))
//      i += 1
//    }
//  }
//
//  @Benchmark
//  def benchmark = {
//    var i = 0
//    while (i < size / 10) { set.remove(keys(i * 10)); i += 1 }
//  }
//}

@State(Scope.Thread)
class SetRemoveStdlibBenchmark {

  import Benchmark._

  var set: StdlibSet[Key] = _

  @Setup(Level.Invocation)
  def setup = {
    set = new StdlibSet[Key] { override val initialSize = Benchmark.initialSize }
    var i = 0
    while (i < size) {
      set.add(keys(i))
      i += 1
    }
  }

  @Benchmark
  def benchmark = {
    var i = 0
    while (i < size / 10) { set.remove(keys(i * 10)); i += 1 }
  }
}

@State(Scope.Thread)
class SetRemoveJavalibBenchmark {

  import Benchmark._

  var set: JavaSet[Key] = _

  @Setup(Level.Invocation)
  def setup = {
    set = new JavaSet[Key](initialSize)
    var i = 0
    while (i < size) {
      set.add(keys(i))
      i += 1
    }
  }

  @Benchmark
  def benchmark = {
    var i = 0
    while (i < size / 10) { set.remove(keys(i * 10)); i += 1 }
  }
}
