package benchmark

import scala.util.Random

object IntBenchmark {

  val size = Benchmark.size

  val random = Random

  val keys: Array[Int] = {
    val keys = new Array[Int](size)
    var i = 0
    while (i < size) {
      keys(i) = random.nextInt
      i += 1
    }
    keys
  }

  val initialSize = {
    var _size = 1
    while (_size < size) _size *= 2
    _size
  }
}
