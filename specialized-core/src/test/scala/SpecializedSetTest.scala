package test

import offheap.collection._
import org.scalatest.{BeforeAndAfter, FunSuite}

import HashEq.Implicits._

class SpecializedSetTest
    extends FunSuite
    with BeforeAndAfter
    with SetTest[Set_Int] {
  def provideSet_Int: Set_Int = new HashSet_Int

  test("intersect") {
    val other = provideSet_Int
    5 to 15 foreach (other.add(_))

    val intersect = set intersect other
    assert(intersect.size == 6)
    1 to 4 foreach (i => assert(!intersect(i)))
    5 to 10 foreach (i => assert(intersect(i)))
    11 to 15 foreach (i => assert(!intersect(i)))
  }

  test("union") {
    val other = provideSet_Int
    5 to 15 foreach (other.add(_))

    val union = set union other
    assert(union.size == 15)
    1 to 15 foreach (i => assert(union(i)))
  }

  test("diff") {
    val other = provideSet_Int
    5 to 15 foreach (set.add(_))
    5 to 10 foreach (other.add(_))

    val diff = set diff other
    assert(diff.size == 9)
    1 to 4 foreach (i => assert(diff(i)))
    5 to 10 foreach (i => assert(!diff(i)))
    11 to 15 foreach (i => assert(diff(i)))
  }

  test("compact") {
    val set = provideSet_Int
    1 to 50 foreach (set.add(_))
    assert(set.capacity == 64)

    1 to 25 foreach (set.remove(_))
    assert(set.capacity == 64)
    set.compact
    assert(set.capacity == 32)

    25 to 40 foreach (set.remove(_))
    assert(set.capacity == 32)
    set.compact
    assert(set.capacity == 16)

    assert(set.size == 10)
    41 to 50 foreach (i => assert(set(i)))
  }
}
