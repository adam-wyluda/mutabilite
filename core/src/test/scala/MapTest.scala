package test

import offheap.collection._
import org.scalatest.{BeforeAndAfter, FunSuite}
import scala.collection.{ mutable => stdlib }

class MapTest extends FunSuite with BeforeAndAfter {
  var map: Map_Int_Object[String] = _

  before {
    map = new HashMap_Int_Object[String]
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
    assert(map.notEmpty)
    assert(new HashMap_Int_Object[String].empty)
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
    1 to 3 foreach (i => assert(map(i) == expected(i)))
    assert(map.get(0).isEmpty)
    assert(map.get(4).isEmpty)
  }

  test("put") {
    map.put(4, "for")
    assert(map.size == 4)
    assert(map(4) == "for")

    map.put(4, "four")
    assert(map.size == 4)
    assert(map(4) == "four")
  }

  test("put and apply") {
    val map = new HashMap_Object_Int[String]
    1 to 100 foreach { i =>
      map.put(i toString, i)
    }
    1 to 100 foreach { i =>
      val opt = map.get(i toString)
      assert(opt.nonEmpty)
      assert(opt.get == i)
    }
  }

  test("remove") {
    val previous = map.remove(2)
    assert(map.size == 2)
    assert(map.get(2).isEmpty)
  }

  test("put and remove") {
    val map = new HashMap_Object_Int[String]
    1 to 100 foreach { i =>
      map.put(i toString, i)
    }
    assert(map.size == 100)
    1 to 100 by 2 foreach { i =>
      assert(map.contains(i toString))
      map.remove(i toString)
    }
    assert(map.size == 50)
    1 to 100 by 2 foreach { i =>
      map.remove(i toString)
    }
    assert(map.size == 50)
    2 to 100 by 2 foreach { i =>
      val opt = map.get(i toString)
      assert(opt.nonEmpty)
      assert(opt.get == i)
    }
  }

  test("keys") {
    val keys = map.keys

    assert(keys.size == 3)
    1 to 3 foreach (i => assert(keys(i)))
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

  test("compact") {
    val map = new HashMap_Int_Int
    1 to 50 foreach (i => map.put(i, i * i))
    assert(map.capacity == 128)

    1 to 25 foreach (map.remove(_))
    assert(map.capacity == 128)
    map.compact
    assert(map.capacity == 32)

    25 to 40 foreach (map.remove(_))
    assert(map.capacity == 32)
    map.compact
    assert(map.capacity == 16)

    assert(map.size == 10)
    41 to 50 foreach (i => assert(map(i) == i * i))
  }
}
