package benchmark

import org.openjdk.jmh.annotations._
import offheap.collection._
import org.openjdk.jmh.infra.Blackhole

import scala.util.Random
import scala.collection.mutable.{ HashSet => StdlibSet }

@State(Scope.Thread)
class SetBenchmark {

  val set: Set[Int] = {
    val set = new Set[Int]
    1 to 10000 foreach (set.add(_))
    set
  }

  val stdSet: StdlibSet[Int] = {
    val set = StdlibSet[Int]()
    1 to 10000 foreach (set.add(_))
    set
  }

  val random = Random

  @Benchmark
  def contains = set(random.nextInt(20000))

  @Benchmark
  def containsStdlib = stdSet(random.nextInt(20000))

  @Benchmark
  def add = {
    val s = new Set[Int]
    var i = 0
    while (i < 10000) {
      s.add(random.nextInt(20000))
      i += 1
    }
  }

  @Benchmark
  def addStdlib = {
    val s = StdlibSet[Int]()
    var i = 0
    while (i < 10000) {
      s.add(random.nextInt(20000))
      i += 1
    }
  }

  @Benchmark
  def foreach(blackhole: Blackhole) = set foreach (blackhole.consume(_))

  @Benchmark
  def foreachStdlib(blackhole: Blackhole) = stdSet foreach (blackhole.consume(_))
}

@State(Scope.Thread)
class SetRemoveBenchmark {

  var set: Set[Int] = _

  @Setup(Level.Invocation)
  def setup = {
    set = new Set[Int]
    var i = 0
    while (i < 10000) { set.add(i); i += 1 }
  }

  @Benchmark
  def benchmark = {
    var i = 0
    while (i < 10000) { set.remove(i); i += 1 }
  }
}

@State(Scope.Thread)
class SetStdlibRemoveBenchmark {

  var set: StdlibSet[Int] = _

  @Setup(Level.Invocation)
  def setup = {
    set = StdlibSet[Int]()
    var i = 0
    while (i < 10000) { set.add(i); i += 1 }
  }

  @Benchmark
  def benchmark = {
    var i = 0
    while (i < 10000) { set.remove(i); i += 1 }
  }
}
