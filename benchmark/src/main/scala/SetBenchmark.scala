package benchmark

import org.openjdk.jmh.annotations._
import offheap.collection._
import org.openjdk.jmh.infra.Blackhole

import scala.util.Random
import scala.collection.mutable.{HashSet => StdlibSet}

object SetBenchmark {

  val setSize = 10000
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
    def generate =
      new Key(random.nextInt(), random.nextInt(), random.nextInt())
  }

  val keys: Array[Key] = {
    val keys = new Array[Key](setSize)
    var i = 0
    while (i < setSize) {
      keys(i) = Key.generate
      i += 1
    }
    keys
  }

  val initialSetSize = {
    var size = 1
    while (size < setSize) size *= 2
    size
  }
}

@State(Scope.Thread)
class SetBenchmark {

  import SetBenchmark._

  val set: MapSet[Key] = {
    val set = new MapSet[Key](initialSetSize)
    var i = 0
    while (i < setSize) {
      set.add(keys(i))
      i += 1
    }
    set
  }

  val stdSet: StdlibSet[Key] = {
    val set = StdlibSet[Key]()
    var i = 0
    while (i < setSize) {
      set.add(keys(i))
      i += 1
    }
    set
  }

  var randKey: Key = _
  var nonExistingKey: Key = _

  @Setup(Level.Invocation)
  def setup = {
    randKey = keys(random.nextInt(setSize))
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
    val s = new MapSet[Key](initialSetSize)
    var i = 0
    while (i < setSize) {
      s.add(keys(i))
      i += 1
    }
  }

  @Benchmark
  def addStdlib = {
    val s = StdlibSet[Key]()
    var i = 0
    while (i < setSize) {
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

  import SetBenchmark._

  var set: MapSet[Key] = _

  @Setup(Level.Invocation)
  def setup = {
    set = new MapSet[Key](initialSetSize)
    var i = 0
    while (i < setSize) {
      set.add(keys(i))
      i += 1
    }
  }

  @Benchmark
  def benchmark = {
    var i = 0
    while (i < setSize / 10) { set.remove(keys(i * 10)); i += 1 }
  }
}

@State(Scope.Thread)
class SetRemoveStdlibBenchmark {

  import SetBenchmark._

  var set: StdlibSet[Key] = _

  @Setup(Level.Invocation)
  def setup = {
    set = StdlibSet[Key]()
    var i = 0
    while (i < setSize) {
      set.add(keys(i))
      i += 1
    }
  }

  @Benchmark
  def benchmark = {
    var i = 0
    while (i < setSize / 10) { set.remove(keys(i * 10)); i += 1 }
  }
}
