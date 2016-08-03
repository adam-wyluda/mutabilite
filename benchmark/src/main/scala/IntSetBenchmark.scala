package benchmark

import org.openjdk.jmh.annotations._
import offheap.collection._
import offheap.collection.ops._
import org.openjdk.jmh.infra.Blackhole

import scala.collection.mutable.{HashSet => StdlibSet}

@State(Scope.Thread)
class IntSetBenchmark {

  import Benchmark._

  val specSet: HashSet_Int = {
    val set = new HashSet_Int(initialSize)
    var i = 0
    while (i < size) {
      set.add(i)
      i += 1
    }
    set
  }

  val deboxSet: debox.Set[Int] = {
    val set = debox.Set.empty[Int]
    var i = 0
    while (i < size) {
      set.add(i)
      i += 1
    }
    set
  }

  val stdSet: StdlibSet[Int] = {
    val set = StdlibSet[Int]()
    var i = 0
    while (i < size) {
      set.add(i)
      i += 1
    }
    set
  }

  var randKey: Int = _
  var nonExistingKey: Int = _

  @Setup(Level.Invocation)
  def setup = {
    randKey = random.nextInt(size)
    nonExistingKey = randKey + size
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
    val s = new HashSet_Int(initialSize = 16)
    var i = 0
    while (i < size) {
      s.add(i)
      i += 1
    }
  }

  @Benchmark
  def addDebox = {
    val s = debox.Set.ofSize[Int](16)
    var i = 0
    while (i < size) {
      s.add(i)
      i += 1
    }
  }

  @Benchmark
  def addStdlib = {
    val s = StdlibSet[Int]()
    var i = 0
    while (i < size) {
      s.add(i)
      i += 1
    }
  }

  @Benchmark
  def foreachMacro(blackhole: Blackhole) =
    specSet foreachMacro (blackhole.consume(_))

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
  def mapSpecialized = specSet map (_ + 1)

  @Benchmark
  def mapDebox = deboxSet map (_ + 1)

  @Benchmark
  def mapStdlib = stdSet map (_ + 1)

  @Benchmark
  def flatMapFold =
    specSet.fold (new HashSet_Int) { (r, i) =>
      var j = 0
      while (j < 5) { r.add(i + j); j += 1 }
      r
    }

  @Benchmark
  def flatMapSpecialized =
    specSet flatMap { i =>
      val r = new HashSet_Int
      var j = 0
      while (j < 5) { r.add(i + j); j += 1 }
      r
    }

  @Benchmark
  def flatMapStdlib =
    stdSet flatMap { i =>
      val r = StdlibSet[Int]()
      var j = 0
      while (j < 5) { r.add(i + j); j += 1 }
      r
    }

  @Benchmark
  def filterSpecialized = specSet filter (_ % 2 == 0)

  @Benchmark
  def filterStdlib = stdSet filter (_ % 2 == 0)
}

@State(Scope.Thread)
class IntSetRemoveSpecializedBenchmark {

  import Benchmark._

  var set: HashSet_Int = _

  @Setup(Level.Invocation)
  def setup = {
    set = new HashSet_Int(initialSize)
    var i = 0
    while (i < size) {
      set.add(i)
      i += 1
    }
  }

  @Benchmark
  def benchmark = {
    var i = 0
    while (i < size / 10) { set.remove(i * 10); i += 1 }
  }
}

@State(Scope.Thread)
class IntSetRemoveDeboxBenchmark {

  import Benchmark._

  var set: debox.Set[Int] = _

  @Setup(Level.Invocation)
  def setup = {
    set = debox.Set.ofSize[Int](initialSize)
    var i = 0
    while (i < size) {
      set.add(i)
      i += 1
    }
  }

  @Benchmark
  def benchmark = {
    var i = 0
    while (i < size / 10) { set.remove(i * 10); i += 1 }
  }
}

@State(Scope.Thread)
class IntSetRemoveStdlibBenchmark {

  import Benchmark._

  var set: StdlibSet[Int] = _

  @Setup(Level.Invocation)
  def setup = {
    set = StdlibSet[Int]()
    var i = 0
    while (i < size) {
      set.add(i)
      i += 1
    }
  }

  @Benchmark
  def benchmark = {
    var i = 0
    while (i < size / 10) { set.remove(i * 10); i += 1 }
  }
}
