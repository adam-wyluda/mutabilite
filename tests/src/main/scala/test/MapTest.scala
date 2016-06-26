package test

import offheap.collection._
import org.scalatest.{BeforeAndAfter, FunSuite}

import scala.{collection => stdlib}

trait MapTest { this: FunSuite with BeforeAndAfter =>

  def provideMap_Int_Object: Map[Int, Object]
  def provideMap_Object_Int: Map[Object, Int]

  var map: Map[Int, Object] = _

  before {
    map = provideMap_Int_Object
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
    assert(provideMap_Int_Object.empty)
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
    1 to 3 foreach (i => assert(map(i).get == expected(i)))
    assert(map(0).empty)
    assert(map(4).empty)
  }

  test("put") {
    val nothing = map.put(4, "for")
    assert(map.size == 4)
    assert(nothing.empty)
    assert(map(4).get == "for")

    val previous = map.put(4, "four")
    assert(map.size == 4)
    assert(previous.get == "for")
    assert(map(4).get == "four")
  }

  test("put and apply") {
    val map = provideMap_Object_Int
    1 to 100 foreach { i =>
      map.put(i toString, i)
    }
    1 to 100 foreach { i =>
      val opt = map(i toString)
      assert(opt.notEmpty)
      assert(opt.get == i)
    }
  }

  test("remove") {
    val previous = map.remove(2)
    assert(map.size == 2)
    assert(previous.get == "two")
    assert(map(2).empty)

    val nothing = map.remove(2)
    assert(map.size == 2)
    assert(nothing.empty)
    assert(map(2).empty)
  }

  test("put and remove") {
    val map = provideMap_Object_Int
    1 to 100 foreach { i =>
      map.put(i toString, i)
    }
    assert(map.size == 100)
    1 to 100 by 2 foreach { i =>
      assert(map.contains(i toString))
      val opt = map.remove(i toString)
      assert(opt.notEmpty)
      assert(opt.get == i)
    }
    assert(map.size == 50)
    1 to 100 by 2 foreach { i =>
      val opt = map.remove(i toString)
      assert(opt.empty)
    }
    assert(map.size == 50)
    2 to 100 by 2 foreach { i =>
      val opt = map(i toString)
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
    0 until values.size foreach (i => assert(values(i) == expected(i + 1)))
  }

  test("contains") {
    1 to 3 foreach (i => assert(map.contains(i)))
    4 to 6 foreach (i => assert(!map.contains(i)))
  }
}
