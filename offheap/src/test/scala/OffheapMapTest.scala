package test

import scala.{ collection => stdlib }
import offheap.collection._
import org.scalatest.{BeforeAndAfter, FunSuite}

import HashEq.Implicits._

class OffheapMapTest extends FunSuite with BeforeAndAfter {

  implicit val allocator = scala.offheap.malloc

  var map: OffheapHashMap_Int_Int = _

  before {
    map = OffheapMap_Int_Int.create()
    map.put(1, 1)
    map.put(2, 22)
    map.put(3, 33)
    map.put(2, 2)
    map.put(3, 2)
  }

  test("isEmpty") {
    assert(map.notEmpty)
    assert(OffheapMap_Int_Int.create().empty)
  }

  test("size") {
    assert(map.size == 3)
  }

  test("foreach") {
    var keys = stdlib.Set[Int]()
    map foreach {
      case (k, v) => {
        assert(v == k)
        keys += k
      }
    }
    assert(keys.size == 3)
    1 to 3 foreach (i => assert(keys(i)))
  }

  test("apply") {
    1 to 3 foreach (i => assert(map(i).get == i))
    assert(map(0).empty)
    assert(map(4).empty)
  }

  test("put") {
    val nothing = map.put(4, 44)
    assert(map.size == 4)
    assert(nothing.empty)
    assert(map(4).get == 4)

    val previous = map.put(4, 4)
    assert(map.size == 4)
    assert(previous.get == 44)
    assert(map(4).get == 4)
  }

  test("put and apply") {
    val map = OffheapMap_Int_Int.create()
    1 to 100 foreach { i =>
      map.put(i, i)
    }
    assert(map.size == 100)
    1 to 100 foreach { i =>
      val opt = map(i)
      assert(opt.notEmpty)
      assert(opt.get == i)
    }
  }

  test("remove") {
    val previous = map.remove(2)
    assert(map.size == 2)
    assert(previous.get == 2)
    assert(map(2).empty)

    val nothing = map.remove(2)
    assert(map.size == 2)
    assert(nothing.empty)
    assert(map(2).empty)
  }

  test("put and remove") {
    val map = OffheapMap_Int_Int.create()
    1 to 100 foreach { i =>
      map.put(i, i)
    }
    assert(map.size == 100)
    1 to 100 by 2 foreach { i =>
      assert(map.contains(i))
      val opt = map.remove(i)
      assert(opt.notEmpty)
      assert(opt.get == i)
    }
    assert(map.size == 50)
    1 to 100 by 2 foreach { i =>
      val opt = map.remove(i)
      assert(opt.empty)
    }
    assert(map.size == 50)
    2 to 100 by 2 foreach { i =>
      val opt = map(i)
      assert(opt.notEmpty)
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
    0 until values.size foreach (i => assert(values(i) == i + 1))
  }

  test("contains") {
    1 to 3 foreach (i => assert(map.contains(i)))
    4 to 6 foreach (i => assert(!map.contains(i)))
  }
}
