package test

import offheap.collection._
import org.scalatest.FunSuite

class SetOpsTest extends FunSuite {
  test("map int to string") {
    val set: Set_Int = new HashSet_Int
    1 to 3 foreach (set.add(_))

    val mapped = set map { _ toString }
    val test: HashSet_Object[String] = mapped
    assert(mapped.size == 3)
    1 to 3 foreach (i => assert(mapped(i toString)))
  }

  test("map string to int") {
    val set: Set_Object[String] = new HashSet_Object[String]
    1 to 3 foreach (i => set.add(i toString))

    val mapped = set map { Integer.parseInt(_) }
    val test: HashSet_Int = mapped
    assert(mapped.size == 3)
    1 to 3 foreach (i => assert(mapped(i)))
  }

  test("map int") {
    val set: Set_Int = new HashSet_Int
    1 to 3 foreach (set.add(_))

    val mapped = set.map(i => i * 2)
    val test: HashSet_Int = mapped
    assert(mapped.size == 3)
    1 to 3 foreach (i => assert(mapped(i * 2)))
  }

  test("flatMap int") {
    val set = new HashSet_Int
    1 to 5 by 2 foreach (set.add(_))

    val mapped = set flatMap { i =>
      val r = new HashSet_Int
      r.add(i)
      r.add(i + 1)
      r
    }
    val test: HashSet_Int = mapped

    assert(mapped.size == 6)
    1 to 6 foreach (i => assert(mapped(i)))
  }

  test("map") {
    val set: Set_Int = new HashSet_Int
    1 to 3 foreach (set.add(_))

    val mapped = set.map(i => i * 2.0f)
    val test: HashSet_Float = mapped
    assert(mapped.size == 3)
    1 to 3 foreach (i => assert(mapped(i * 2.0f)))
  }

  test("flatMap") {
    val set: Set_Int = new HashSet_Int
    1 to 5 by 2 foreach (set.add(_))

    val mapped = set flatMap { i =>
      val r = new HashSet_Float
      r.add(i)
      r.add(i + 1)
      r
    }
    val test: HashSet_Float = mapped

    assert(mapped.size == 6)
    1 to 6 foreach (i => assert(mapped(i toFloat)))
  }

  test("filter") {
    val set: Set_Int = new HashSet_Int
    1 to 10 foreach (set.add(_))

    val filtered = set filter (i => i % 2 == 0)
    val test: HashSet_Int = filtered

    assert(filtered.size == 5)
    2 to 10 by 2 foreach (i => assert(filtered(i)))
  }

  test("foreachMacro") {
    val set: Set_Int = new HashSet_Int
    1 to 10 foreach (set.add(_))

    var sum = 0
    set foreach (sum += _)
    assert(sum == 10 * 11 / 2)
  }

  test("fold") {
    val set: Set_Int = new HashSet_Int
    1 to 3 foreach (set.add(_))

    assert(set.fold(5)(_ + _) == 11)
    assert(new HashSet_Int().fold[Int](3)((_, _) => 1) == 3)
  }

  test("reduce") {
    val set: Set_Int = new HashSet_Int
    1 to 3 foreach (set.add(_))

    assert(set.reduce(_ + _) == 6)
  }

  test("forall") {
    val set: Set_Int = new HashSet_Int
    2 to 100 by 2 foreach (set.add(_))

    assert(set.forall(_ % 2 == 0))
    assert(!set.forall(_ > 50))
  }

  test("exists") {
    val set: Set_Int = new HashSet_Int
    2 to 100 by 2 foreach (set.add(_))

    assert(set.exists(_ == 62))
    assert(!set.exists(_ % 2 == 1))
  }
}
