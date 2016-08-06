package test

import offheap.collection.{BufferSeq_Int, _}
import org.scalatest.{BeforeAndAfter, FunSuite}

class SeqTest extends FunSuite with BeforeAndAfter {
  var seq: Seq_Int = _

  before {
    seq = new BufferSeq_Int
    1 to 10 foreach (seq.append(_))
  }

  test("isEmpty") {
    assert(seq.nonEmpty)
    assert(new BufferSeq_Int().isEmpty)
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
    val seq = new BufferSeq_Int

    assert(seq.isEmpty)
    1 to 3 foreach (seq.append(_))
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

  test("remove at capacity") {
    val newSeq = new BufferSeq_Int
    1 to newSeq.capacity foreach (newSeq.append(_))

    assert(newSeq.size == newSeq.capacity)
    assert(newSeq.remove(newSeq.size - 1) == newSeq.capacity)
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

  test("compact") {
    val seq = new BufferSeq_Int
    1 to 50 foreach (seq.append(_))
    assert(seq.capacity == 64)

    1 to 25 foreach (_ => seq.remove(0))
    assert(seq.capacity == 64)
    seq.compact
    assert(seq.capacity == 32)

    1 to 15 foreach (_ => seq.remove(0))
    assert(seq.capacity == 32)
    seq.compact
    assert(seq.capacity == 16)

    assert(seq.size == 10)
    0 until 10 foreach (i => assert(seq(i) == i + 41))
  }
}
