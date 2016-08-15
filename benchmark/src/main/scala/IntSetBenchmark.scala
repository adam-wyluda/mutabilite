package benchmark

import java.util.function.Consumer

import org.openjdk.jmh.annotations._
import offheap.collection._
import org.openjdk.jmh.infra.Blackhole

import scala.collection.mutable.{HashSet => StdlibSet}
import java.util.{HashSet => JavaSet}

@State(Scope.Thread)
class IntSetBenchmark {

  import IntBenchmark._

  val specSet: HashSet_Int = {
    val set = new HashSet_Int(initialSize = initialSize)
    var i = 0
    while (i < size) {
      set.add(keys(i))
      i += 1
    }
    set
  }

  val deboxSet: debox.Set[Int] = {
    val set = debox.Set.ofSize[Int](size)
    var i = 0
    while (i < size) {
      set.add(keys(i))
      i += 1
    }
    set
  }

  val stdSet: StdlibSet[Int] = {
    val set = new StdlibSet[Int] { override val initialSize = IntBenchmark.initialSize }
    var i = 0
    while (i < size) {
      set.add(keys(i))
      i += 1
    }
    set
  }

  val javaSet: JavaSet[Int] = {
    val set = new JavaSet[Int](initialSize)
    var i = 0
    while (i < size) {
      set.add(keys(i))
      i += 1
    }
    set
  }

  var randKey: Int = _
  var nonExistingKey: Int = _

  @Setup(Level.Invocation)
  def setup = {
    randKey = keys(random.nextInt(size))
    nonExistingKey = random.nextInt
  }

  @Benchmark
  def containsExistingSpecialized = specSet(randKey)

  @Benchmark
  def containsExistingDebox = deboxSet(randKey)

  @Benchmark
  def containsExistingStdlib = stdSet(randKey)

  @Benchmark
  def containsExistingJavalib = javaSet.contains(randKey)

  @Benchmark
  def containsNonExistingSpecialized = specSet(nonExistingKey)

  @Benchmark
  def containsNonExistingDebox = deboxSet(nonExistingKey)

  @Benchmark
  def containsNonExistingStdlib = stdSet(nonExistingKey)

  @Benchmark
  def containsNonExistingJavalib = javaSet.contains(nonExistingKey)

  @Benchmark
  def addSpecialized = {
    val s = new HashSet_Int(initialSize = initialSize)
    var i = 0
    while (i < size) {
      s.add(keys(i))
      i += 1
    }
  }

  @Benchmark
  def addDebox = {
    val s = debox.Set.ofSize[Int](size)
    var i = 0
    while (i < size) {
      s.add(keys(i))
      i += 1
    }
  }

  @Benchmark
  def addStdlib = {
    val s = new StdlibSet[Int] { override val initialSize = IntBenchmark.initialSize }
    var i = 0
    while (i < size) {
      s.add(keys(i))
      i += 1
    }
  }

  @Benchmark
  def addJavalib = {
    val s = new JavaSet[Int](initialSize)
    var i = 0
    while (i < size) {
      s.add(keys(i))
      i += 1
    }
  }

  @Benchmark
  def foreachSpecialized(blackhole: Blackhole) =
    specSet foreach (blackhole.consume(_))

  @Benchmark
  def foreachDebox(blackhole: Blackhole) =
    deboxSet foreach (blackhole.consume(_))

  @Benchmark
  def foreachStdlib(blackhole: Blackhole) =
    stdSet foreach (blackhole.consume(_))

  @Benchmark
  def foreachJavalib(blackhole: Blackhole) =
    javaSet.forEach(new Consumer[Int] {
      def accept(t: Int): Unit = blackhole.consume(t)
    })

  @Benchmark
  def mapSpecialized = specSet map (_ + 1)

  @Benchmark
  def mapDebox = deboxSet map (_ + 1)

  @Benchmark
  def mapStdlib = stdSet map (_ + 1)
}

@State(Scope.Thread)
class IntSetRemoveSpecializedBenchmark {

  import IntBenchmark._

  var set: HashSet_Int = _

  @Setup(Level.Invocation)
  def setup = {
    set = new HashSet_Int(initialSize)
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
class IntSetRemoveDeboxBenchmark {

  import IntBenchmark._

  var set: debox.Set[Int] = _

  @Setup(Level.Invocation)
  def setup = {
    set = debox.Set.ofSize[Int](size)
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
class IntSetRemoveStdlibBenchmark {

  import IntBenchmark._

  var set: StdlibSet[Int] = _

  @Setup(Level.Invocation)
  def setup = {
    set = new StdlibSet[Int] { override val initialSize = IntBenchmark.initialSize }
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
class IntSetRemoveJavalibBenchmark {

  import IntBenchmark._

  var set: JavaSet[Int] = _

  @Setup(Level.Invocation)
  def setup = {
    set = new JavaSet[Int](initialSize)
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
