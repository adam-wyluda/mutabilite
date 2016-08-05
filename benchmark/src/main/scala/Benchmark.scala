package benchmark

import scala.util.Random

object Benchmark {

  //  With load factor of 0.65, this should not trigger growth
  val size = 80000

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
    val keys = new Array[Key](size)
    var i = 0
    while (i < size) {
      keys(i) = Key.generate
      i += 1
    }
    keys
  }

  val initialSize = {
    var _size = 1
    while (_size * 2 < size) _size *= 2
    _size * 2
  }
}
