package benchmark

import org.openjdk.jmh.annotations._
import offheap.collection._
import HashEq.Implicits._
import org.openjdk.jmh.infra.Blackhole

import scala.collection.mutable.{HashSet => StdlibSet}

@State(Scope.Thread)
class IntSetBenchmark {

  import Benchmark._

//  implicit val props = Region.Props(Pool(pageSize = 4 * 1024 * 1024, chunkSize = 16 * 1024 * 1024))
//  val malloc = scala.offheap.malloc
//
//  val offheapSet: OffheapHashSet_Int = {
//    implicit val alloc = malloc
//    val set = OffheapSet_Int.create(initialSize)
//    var i = 0
//    while (i < size) {
//      set.add(i)
//      i += 1
//    }
//    set
//  }

  val specSet: HashSet_Int = {
    val set = new HashSet_Int(initialSize)
    var i = 0
    while (i < size) {
      set.add(i)
      i += 1
    }
    set
  }

  val genericSet: HashSet[Int] = {
    val set = new HashSet[Int](initialSize)
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

//  var freedSet: OffheapHashSet_Int = _

  var randKey: Int = _
  var nonExistingKey: Int = _

//  var region: Region = _

  @Setup(Level.Invocation)
  def setup = {
    randKey = random.nextInt(size)
    nonExistingKey = randKey + size
//    freedSet = OffheapHashSet_Int.empty
//    region = Region.open
  }

//  @TearDown(Level.Invocation)
//  def tearDown = {
//    if (freedSet.nonEmpty) freedSet.free(malloc)
//    region.close
//  }

//  @Benchmark
//  def containsExistingOffheap = offheapSet(randKey)

  @Benchmark
  def containsExistingSpecialized = specSet(randKey)

  @Benchmark
  def containsExistingGeneric = genericSet(randKey)

  @Benchmark
  def containsExistingStdlib = stdSet(randKey)

//  @Benchmark
//  def containsNonExistingOffheap = offheapSet(nonExistingKey)

  @Benchmark
  def containsNonExistingSpecialized = specSet(nonExistingKey)

  @Benchmark
  def containsNonExistingGeneric = genericSet(nonExistingKey)

  @Benchmark
  def containsNonExistingStdlib = stdSet(nonExistingKey)

//  @Benchmark
//  def addOffheap = {
//    implicit val alloc = malloc
//    val freedSet = OffheapSet_Int.create(initialSize = 16)
//    var i = 0
//    while (i < size) {
//      freedSet.add(i)
//      i += 1
//    }
//  }
//
//  @Benchmark
//  def addRegion = {
//    implicit val alloc = region
//    val s = OffheapSet_Int.create(initialSize = 16)
//    var i = 0
//    while (i < size) {
//      s.add(i)
//      i += 1
//    }
//  }

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
  def addGeneric = {
    val s = new HashSet[Int](initialSize = 16)
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

//  @Benchmark
//  def foreachOffheap(blackhole: Blackhole) =
//    offheapSet foreach (blackhole.consume(_))

  @Benchmark
  def foreachMacro(blackhole: Blackhole) =
    specSet foreachMacro (blackhole.consume(_))

  @Benchmark
  def foreachSpecialized(blackhole: Blackhole) =
    specSet foreach (blackhole.consume(_))

  @Benchmark
  def foreachGeneric(blackhole: Blackhole) =
    genericSet foreachGeneric (blackhole.consume(_))

  @Benchmark
  def foreachStdlib(blackhole: Blackhole) =
    stdSet foreach (blackhole.consume(_))

  @Benchmark
  def mapSpecialized = specSet map (_ + 1)

  @Benchmark
  def mapGeneric = genericSet map (_ + 1)

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
  def flatMapGeneric =
    genericSet flatMap { i =>
      val r = new HashSet[Int]
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
  def filterGeneric = genericSet filter (_ % 2 == 0)

  @Benchmark
  def filterStdlib = stdSet filter (_ % 2 == 0)
}

//@State(Scope.Thread)
//class IntSetRemoveOffheapBenchmark {
//
//  import Benchmark._
//
//  implicit val allocator = scala.offheap.malloc
//
//  var set: OffheapHashSet_Int = _
//
//  @Setup(Level.Invocation)
//  def setup = {
//    set = OffheapSet_Int.create(initialSize)
//    var i = 0
//    while (i < size) {
//      set.add(i)
//      i += 1
//    }
//  }
//
//  @TearDown(Level.Invocation)
//  def tearDown = set.free
//
//  @Benchmark
//  def benchmark = {
//    var i = 0
//    while (i < size / 10) { set.remove(i * 10); i += 1 }
//  }
//}

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
class IntSetRemoveGenericBenchmark {

  import Benchmark._

  var set: HashSet[Int] = _

  @Setup(Level.Invocation)
  def setup = {
    set = new HashSet[Int](initialSize)
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
