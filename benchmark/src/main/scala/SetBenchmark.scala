package benchmark

import org.openjdk.jmh.annotations._
import offheap.collection._
import org.openjdk.jmh.infra.Blackhole

import scala.collection.mutable.{HashSet => StdlibSet}

@State(Scope.Thread)
class SetBenchmark {

  import Benchmark._

  val specSet: HashSet_Object[Key] = {
    val set = new HashSet_Object[Key](initialSize)
    var i = 0
    while (i < size) {
      set.add(keys(i))
      i += 1
    }
    set
  }

  val deboxSet: debox.Set[Key] = {
    val set = debox.Set.ofSize[Key](initialSize)
    var i = 0
    while (i < size) {
      set.add(keys(i))
      i += 1
    }
    set
  }

  val stdSet: StdlibSet[Key] = {
    val set = StdlibSet[Key]()
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

  @Benchmark
  def containsExistingDebox = deboxSet(randKey)

  @Benchmark
  def containsExistingStdlib = stdSet(randKey)

  @Benchmark
  def containsNonExistingSpecialized = specSet(nonExistingKey)

  @Benchmark
  def containsNonExistingDebox = deboxSet(nonExistingKey)

  @Benchmark
  def containsNonExistingStdlib = stdSet(nonExistingKey)

  @Benchmark
  def addSpecialized = {
    val s = new HashSet_Object[Key](initialSize = initialSize)
    var i = 0
    while (i < size) {
      s.add(keys(i))
      i += 1
    }
  }

  @Benchmark
  def addDebox = {
    val s = debox.Set.ofSize[Key](initialSize)
    var i = 0
    while (i < size) {
      s.add(keys(i))
      i += 1
    }
  }

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
  def foreachSpecialized(blackhole: Blackhole) =
    specSet foreach (blackhole.consume(_))

  @Benchmark
  def foreachDebox(blackhole: Blackhole) =
    deboxSet foreach (blackhole.consume(_))

  @Benchmark
  def foreachStdlib(blackhole: Blackhole) =
    stdSet foreach (blackhole.consume(_))
}

@State(Scope.Thread)
class SetRemoveSpecializedBenchmark {

  import Benchmark._

  var set: HashSet_Object[Key] = _

  @Setup(Level.Invocation)
  def setup = {
    set = new HashSet_Object[Key](initialSize)
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
class SetRemoveDeboxBenchmark {

  import Benchmark._

  var set: debox.Set[Key] = _

  @Setup(Level.Invocation)
  def setup = {
    set = debox.Set.ofSize[Key](initialSize)
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
