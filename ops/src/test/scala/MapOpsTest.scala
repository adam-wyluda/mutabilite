package test

import offheap.collection._
import offheap.collection.ops._
import org.scalatest.{BeforeAndAfter, FunSuite}

class MapOpsTest extends FunSuite {
  test("map (int, string) to float") {
    val map = new HashMap_Int_Object[String]
    1 to 10 foreach (i => map.put(i, i toString))

    val mapped = map map { (k, v) =>
      k.toFloat / v.length
    }
    val test: Seq_Float = mapped

    assert(mapped.size == 10)
    1 to 10 foreach (i =>
          assert(mapped.index(i.toFloat / (i.toString.length)) != -1))
  }

  test("map (string, int) to string") {
    val map = new HashMap_Object_Int[String]
    1 to 10 foreach (i => map.put(i toString, i + 10))

    val mapped = map map { (k, v) =>
      k + v.toString
    }
    val test: Seq_Object[String] = mapped

    assert(mapped.size == 10)
    1 to 10 foreach (i =>
          assert(mapped.index(i.toString + (i + 10).toString) != -1))
  }

  test("mapKeys (int, _) to (string, _)") {
    val map = new HashMap_Int_Int
    1 to 10 foreach (i => map.put(i, i * 10))

    val mapped = map mapKeys (_ toString)
    val test: Map_Object_Int[String] = mapped

    assert(mapped.size == 10)
    1 to 10 foreach (i => assert(mapped.contains(i toString)))
  }

  test("mapKeys (string, _) to (int, _)") {
    val map = new HashMap_Object_Object[String, String]
    1 to 10 foreach (i => map.put(i toString, (i * 10) toString))

    val mapped = map mapKeys (Integer.parseInt(_))
    val test: Map_Int_Object[String] = mapped

    assert(mapped.size == 10)
    1 to 10 foreach (i => assert(mapped.contains(i)))
  }

  test("mapValues (_, int) to (_, string)") {
    val map = new HashMap_Int_Int
    1 to 10 foreach (i => map.put(i, i * 10))

    val mapped = map mapValues (_ toString)
    val test: Map_Int_Object[String] = mapped

    assert(mapped.size == 10)
    1 to 10 foreach (i => assert(mapped(i) == (i * 10).toString))
  }

  test("mapValues (_, string) to (_, int)") {
    val map = new HashMap_Int_Object[String]
    1 to 10 foreach (i => map.put(i, (i * 10) toString))

    val mapped = map mapValues (Integer.parseInt(_))
    val test: Map_Int_Int = mapped

    assert(mapped.size == 10)
    1 to 10 foreach (i => assert(mapped(i) == i * 10))
  }

  test("flatMap") {
    val map = new HashMap_Int_Object[String]
    1 to 10 foreach (i => map.put(i * 3, (i * 3) toString))

    val mapped = map flatMap { (k, v) =>
      val seq = new BufferSeq_Int
      seq.append(k + v.length)
      seq.append(k + v.length + 128)
      seq.append(k + v.length + 512)
      seq
    }
    val test: Seq_Int = mapped

    assert(mapped.size == 30)
    1 to 10 map (_ * 3) foreach { i =>
      assert(mapped.index(i + (i toString).length) != -1)
      assert(mapped.index(i + (i toString).length + 128) != -1)
      assert(mapped.index(i + (i toString).length + 512) != -1)
    }
  }

  test("filter") {
    val map = new HashMap_Int_Object[String]
    1 to 10 foreach (i => map.put(i, i toString))

    val filtered = map filter { (k, _) =>
      k % 2 == 0
    }
    val test: Map_Int_Object[String] = filtered

    assert(filtered.size == 5)
    2 to 10 by 2 foreach (i => assert(filtered(i) == i.toString))
  }

  test("fold") {
    val map = new HashMap_Int_Object[String]
    1 to 5 foreach (i => map.put(i, i toString))

    val sum = map.fold(3) { (acc, k, v) =>
      acc + k + v.length
    }
    assert(sum == 3 + 5 * 6 / 2 + 5)

    assert(new HashMap_Int_Short().fold(7) { (acc, k, v) =>
      acc + k + v
    } == 7)
  }

  test("reduceKeys") {
    val map = new HashMap_Int_Object[String]
    1 to 5 foreach (i => map.put(i, i toString))

    val sum = map.reduceKeys { (acc, k) =>
      acc + k
    }
    assert(sum == 5 * 6 / 2)
  }

  test("reduceValues") {
    val map = new HashMap_Int_Object[String]
    1 to 5 foreach (i => map.put(i, i toString))

    val sum = map.reduceValues { (acc: String, v: String) =>
      acc + v
    }
    "12345" foreach (c => assert(sum.contains(c)))
  }

  test("transformValues") {
    val map = new HashMap_Int_Int
    1 to 10 foreach (i => map.put(i, i))

    map.transformValues(_ * 10)

    1 to 10 foreach (i => assert(map(i) == i * 10))
  }

  test("forall") {
    val map = new HashMap_Int_Int
    1 to 100 foreach (i => map.put(i * 2 + 1, i * 2))

    assert(map.forall((k, v) => k % 2 == 1 && v % 2 == 0))
    assert(map.forall(_ == _ + 1))
    assert(!map.forall((k, _) => k == 30))
  }

  test("exists") {
    val map = new HashMap_Int_Int
    1 to 100 foreach (i => map.put(i * 2 + 1, i * 2))

    assert(map.exists((k, v) => k == 21 && v == 20))
    assert(!map.exists(_ == _))
  }
}
