package test

import offheap.collection._
import org.scalatest.{BeforeAndAfter, FunSuite}

import HashEq.Implicits._

class SpecializedSeqTest extends FunSuite with BeforeAndAfter with SeqTest {
  def provideSeq_Int: BufferSeq_Int = new BufferSeq_Int

  test("compact") {
    val seq = provideSeq_Int
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
