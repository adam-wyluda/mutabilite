package test

import mutabilite._
import org.scalatest.{BeforeAndAfter, FunSuite}

import scala.collection.{mutable => stdlib}

class MapTest extends FunSuite with BeforeAndAfter {
  var map: Map_Int_Object[String] = _

  before {
    map = new Map_Int_Object[String]
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
    assert(new Map_Int_Object[String].isEmpty)
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
    val map = new Map_Object_Int[String]
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
    val map = new Map_Object_Int[String]
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

  test("put and remove 2") {
    for (_ <- 1 to 100) {
      val map = new Map_Int_Int
      val rand = scala.util.Random

      val keys = Array.fill[Int](100000)(rand.nextInt)
      keys foreach (map.put(_, rand.nextInt))
      keys foreach (key => assert(map.contains(key)))
      0 until keys.size by 2 foreach (i => map.remove(keys(i)))
      0 until keys.size by 2 foreach (i => assert(!map.contains(keys(i))))

      val newKeys = Array.fill[Int](100000)(rand.nextInt)
      newKeys foreach (map.put(_, rand.nextInt))
      newKeys foreach (key => assert(map.contains(key)))
      0 until keys.size by 2 foreach (i => map.remove(newKeys(i)))
      0 until keys.size by 2 foreach (i => assert(!map.contains(newKeys(i))))
    }
  }

  test("put and remove 3") {
    val map = new Map_Int_Int
    val rand = scala.util.Random

    val n = 50000
    val vals = Array.fill[Int](n * 2)(rand.nextInt)
    val keyRange = -n until n
    keyRange foreach (i => map.put(i, vals(i + n)))
    assert(map.size == vals.size)
    keyRange foreach { i =>
      assert(map.contains(i))
      assert(map(i) == vals(i + n))
    }
    keyRange by 2 foreach { i =>
      map.remove(i)
      assert(!map.contains(i))
      assert(map.get(i).isEmpty)
    }
    assert(map.size == vals.size / 2)
    keyRange drop 1 by 2 foreach { i =>
      assert(map.contains(i))
      assert(map(i) == vals(i + n))
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
    1 to values.size foreach (i => assert(values.index(expected(i)) != -1))
  }

  test("contains") {
    1 to 3 foreach (i => assert(map.contains(i)))
    4 to 6 foreach (i => assert(!map.contains(i)))
  }

  test("compact") {
    val map = new Map_Int_Int
    1 to 50 foreach (i => map.put(i, i * i))
    assert(map.capacity == 128)

    1 to 25 foreach (map.remove(_))
    assert(map.capacity == 128)
    map.compact
    assert(map.capacity == 64)

    25 to 40 foreach (map.remove(_))
    assert(map.capacity == 64)
    map.compact
    assert(map.capacity == 16)

    assert(map.size == 10)
    41 to 50 foreach (i => assert(map(i) == i * i))
  }
}
