package benchmark

import scala.util.Random

object IntBenchmark {

  val size = Benchmark.size

  val random = Random

  val keys: Array[Int] = Array.fill[Int](size)(random.nextInt(1000000))

  val initialSize = {
    var _size = 1
    while (_size < size) _size *= 2
    _size
  }
}
