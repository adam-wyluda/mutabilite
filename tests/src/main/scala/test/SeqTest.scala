package test

import offheap.collection._
import org.scalatest.{BeforeAndAfter, FunSuite}

trait SeqTest { this: FunSuite with BeforeAndAfter =>

  def provideSeq_Int: Seq[Int]

  var seq: Seq[Int] = _

  before {
    seq = provideSeq_Int
    1 to 10 foreach (seq.append(_))
  }

  test("isEmpty") {
    assert(seq.notEmpty)
    assert(provideSeq_Int.empty)
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
    val seq = provideSeq_Int

    assert(seq.empty)
    1 to 3 foreach (seq.append(_))
    assert(seq.notEmpty)
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
