package test

import scala.{collection => stdlib}
import org.scalatest.{BeforeAndAfter, FunSuite}
import offheap.collection._

class OptTest extends FunSuite {
  val opt = new Opt[Int](25)
  val none = new Opt[Int]

  test("isEmpty") {
    assert(opt.nonEmpty)
    assert(none.isEmpty)
  }

  test("get") {
    assert(opt.get == 25)
    intercept[NoSuchElementException] {
      none.get
    }
  }

  test("foreach") {
    var optSum = 0
    var noneSum = 0

    opt.foreach(optSum += _)
    none.foreach(noneSum += _)

    assert(optSum == 25)
    assert(noneSum == 0)
  }
}

class SeqTest extends FunSuite with BeforeAndAfter {
  var seq: Seq[Int] = _

  before {
    seq = new Seq[Int]
    1 to 10 foreach (seq.append(_))
  }

  test("isEmpty") {
    assert(seq.nonEmpty)
    assert(new Seq[Int].isEmpty)
  }

  test("size") {
    assert(seq.size == 10)
  }

  test("foreach") {
    var sum = 0

    seq.foreach(sum += _)

    assert(sum == 10 * 11 / 2)
  }

  test("apply") {
    for (i <- 0 until 10) {
      assert(seq(i) == i + 1)
    }
  }

  test("append") {
    val seq = new Seq[Int]

    assert(seq.isEmpty)
    seq.append(1, 2, 3)
    assert(seq.nonEmpty)
    assert(seq.size == 3)
  }

  test("update") {
    seq(0) = 100
    assert(seq(0) == 100)
  }

  test("remove") {
    seq.remove(1)
    assert(seq.size == 9)

    assert(seq(0) == 1)
    for (i <- 1 until 9) {
      assert(seq(i) == i + 2)
    }

    seq.remove(seq.size - 1)
    assert(seq.size == 8)
  }

  test("index") {
    for (i <- 1 to 10) {
      assert(seq.index(i) == i - 1)
    }
    assert(seq.index(0) == -1)
  }

  test("insert") {
    seq.insert(1, 50)
    assert(seq.size == 11)

    assert(seq(0) == 1)
    assert(seq(1) == 50)
    for (i <- 2 until 10) {
      assert(seq(i) == i)
    }
  }
}

class SetTest extends FunSuite with BeforeAndAfter {
  var set: Set[Int] = _

  before {
    set = new Set[Int]
    1 to 7 foreach (set.add(_))
    5 to 10 foreach (set.add(_))
  }

  test("isEmpty") {
    assert(set.nonEmpty)
    assert(new Set[Int].isEmpty)
  }

  test("size") {
    assert(set.size == 10)
  }

  test("foreach") {
    var result = List[Int]()
    set.foreach(result ::= _)

    assert(result.size == result.distinct.size)
    assert(result.sum == 10 * 11 / 2)
  }

  test("apply") {
    assert(!set(0))
    1 to 10 foreach (x => assert(set(x)))
    11 to 100 foreach (x => assert(!set(x)))
  }

  test("add") {
    assert(!set.add(5))
    assert(set.add(15))
    assert(set.add(45))

    assert(set.size == 12)
    assert(set(15))
    assert(set(45))
  }

  test("remove") {
    for (i <- 1 to 10 by 2) {
      assert(set.remove(i))
      assert(!set.remove(i))
    }
    assert(set.size == 5)
    1 to 10 by 2 foreach (i => assert(!set(i)))
    2 to 10 by 2 foreach (i => assert(set(i)))
  }

  test("intersect") {
    val other = new Set[Int]
    5 to 15 foreach (other.add(_))

    val intersect = set intersect other
    assert(intersect.size == 6)
    1 to 4 foreach (i => assert(!intersect(i)))
    5 to 10 foreach (i => assert(intersect(i)))
    11 to 15 foreach (i => assert(!intersect(i)))
  }

  test("union") {
    val other = new Set[Int]
    5 to 15 foreach (other.add(_))

    val union = set union other
    assert(union.size == 15)
    1 to 15 foreach (i => assert(union(i)))
  }

  test("diff") {
    val other = new Set[Int]
    5 to 15 foreach (set.add(_))
    5 to 10 foreach (other.add(_))

    val diff = set diff other
    assert(diff.size == 9)
    1 to 4 foreach (i => assert(diff(i)))
    5 to 10 foreach (i => assert(!diff(i)))
    11 to 15 foreach (i => assert(diff(i)))
  }
}

class MapTest extends FunSuite with BeforeAndAfter {
  var map: Map[Int, String] = _

  before {
    map = new Map[Int, String]
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
    assert(new Map[Int, String].isEmpty)
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
