package benchmark

import org.openjdk.jmh.annotations._
import offheap.collection._
import HashEq.Implicits._
import org.openjdk.jmh.infra.Blackhole

import scala.collection.mutable.{HashSet => StdlibSet}

@State(Scope.Thread)
class SetBenchmark {

  import Benchmark._

  val set: ObjectHashSet = {
    val set = new ObjectHashSet(initialSize)
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
  def containsExisting = set(randKey)

  @Benchmark
  def containsExistingStdlib = stdSet(randKey)

  @Benchmark
  def containsNonExisting = set(nonExistingKey)

  @Benchmark
  def containsNonExistingStdlib = stdSet(nonExistingKey)

  @Benchmark
  def add = {
    val s = new ObjectHashSet(initialSize)
    var i = 0
    while (i < size) {
      s.add(keys(i))
      i += 1
    }
  }

  @Benchmark
  def addStdlib = {
    val s = StdlibSet[Key]()
    var i = 0
    while (i < size) {
      s.add(keys(i))
      i += 1
    }
  }

  @Benchmark
  def foreach(blackhole: Blackhole) = set foreach (blackhole.consume(_))

  @Benchmark
  def foreachStdlib(blackhole: Blackhole) =
    stdSet foreach (blackhole.consume(_))
}

@State(Scope.Thread)
class SetRemoveBenchmark {

  import Benchmark._

  var set: ObjectHashSet = _

  @Setup(Level.Invocation)
  def setup = {
    set = new ObjectHashSet(initialSize)
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
    set = StdlibSet[Key]()
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
