package test

import scala.{collection => stdlib}
import org.scalatest.{BeforeAndAfter, FunSuite}
import offheap.collection._

class MapTest extends FunSuite with BeforeAndAfter {
  var map: Map[Int, String] = _

  before {
    map = new NaiveMap[Int, String]
    map.put(1, "one")
    map.put(2, "too")
    map.put(3, "tree")
    map.put(2, "two")
    map.put(3, "three")
  }

  def expected(arg: Int): String = arg match {
    case 1 => "one"
    case 2 => "two"
    case 3 => "three"
  }

  test("isEmpty") {
    assert(map.nonEmpty)
    assert(new NaiveMap[Int, String].isEmpty)
  }

  test("size") {
    assert(map.size == 3)
  }

  test("foreach") {
    var keys = stdlib.Set[Int]()
    map foreach {
      case (k, v) => {
        assert(v == expected(k))
        keys += k
      }
    }
    assert(keys.size == 3)
    1 to 3 foreach (i => assert(keys(i)))
  }

  test("apply") {
    1 to 3 foreach (i => assert(map(i).get() == expected(i)))
    assert(map(0).isEmpty)
    assert(map(4).isEmpty)
  }

  test("put") {
    val nothing = map.put(4, "for")
    assert(map.size == 4)
    assert(nothing.isEmpty)
    assert(map(4).get() == "for")

    val previous = map.put(4, "four")
    assert(map.size == 4)
    assert(previous.get() == "for")
    assert(map(4).get() == "four")
  }

  test("remove") {
    val previous = map.remove(2)
    assert(map.size == 2)
    assert(previous.get() == "two")
    assert(map(2).isEmpty)

    val nothing = map.remove(2)
    assert(map.size == 2)
    assert(nothing.isEmpty)
    assert(map(2).isEmpty)
  }

  test("keys") {
    val keys = map.keys

    assert(keys.size == 3)
    1 to 3 foreach (i => keys(i))
  }

  test("values") {
    val values = map.values

    assert(values.size == 3)
    0 until values.size foreach (i => assert(values(i) == expected(i + 1)))
  }

  test("contains") {
    1 to 3 foreach (i => assert(map.contains(i)))
    4 to 6 foreach (i => assert(!map.contains(i)))
  }
}
