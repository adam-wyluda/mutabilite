package test

import offheap.collection._
import org.scalatest.{BeforeAndAfter, FunSuite}

import HashEq.Implicits._

class SpecializedOptTest extends FunSuite with BeforeAndAfter with OptTest {
  def provideOpt_Int(value: Int): Opt[Int] = new Some_Int(value)
  def provideNone: Opt[Int] = None_Int
}

class SpecializedSeqTest extends FunSuite with BeforeAndAfter with SeqTest {
  def provideSeq_Int: Seq[Int] = new BufferSeq_Int

  test("map int to string") {
    val seq: Seq_Int = new BufferSeq_Int
    1 to 3 foreach (seq.append(_))

    val mapped = seq map { _ toString }
    val test: BufferSeq_Object[String] = mapped
    assert(mapped.size == 3)
    1 to 3 foreach (i => assert(mapped(i - 1) == i.toString))
  }

  test("map string to int") {
    val seq: Seq_Object[String] = new BufferSeq_Object[String]
    1 to 3 foreach (i => seq.append(i toString))

    val mapped = seq map { Integer.parseInt(_) }
    val test: BufferSeq_Int = mapped
    assert(mapped.size == 3)
    1 to 3 foreach (i => assert(mapped(i - 1) == i))
  }

  test("map int") {
    val seq: Seq_Int = new BufferSeq_Int
    1 to 3 foreach (seq.append(_))

    val mapped = seq.map(i => i * 2)
    val test: BufferSeq_Int = mapped
    assert(mapped.size == 3)
    1 to 3 foreach (i => assert(mapped(i - 1) == i * 2))
  }

  test("flatMap int") {
    val seq: Seq_Int = new BufferSeq_Int
    1 to 5 by 2 foreach (seq.append(_))

    val mapped = seq flatMap { i =>
      val r = new BufferSeq_Int
      r.append(i)
      r.append(i + 1)
      r
    }
    val test: BufferSeq_Int = mapped

    assert(mapped.size == 6)
    1 to 6 foreach (i => assert(mapped(i - 1) == i))
  }

  test("map") {
    val seq: Seq_Int = new BufferSeq_Int
    1 to 3 foreach (seq.append(_))

    val mapped = seq.map(i => i * 2.0f)
    val test: Seq_Float = mapped
    assert(mapped.size == 3)
    1 to 3 foreach (i => assert(mapped(i - 1) == i * 2.0f))
  }

  test("flatMap") {
    val seq: Seq_Int = new BufferSeq_Int
    1 to 5 by 2 foreach (seq.append(_))

    val mapped = seq flatMap { i =>
      val r = new BufferSeq_Float
      r.append(i)
      r.append(i + 1)
      r
    }
    val test: BufferSeq_Float = mapped

    assert(mapped.size == 6)
    1 to 6 foreach (i => assert(mapped(i - 1) == i))
  }

  test("filter") {
    val seq: Seq_Int = new BufferSeq_Int
    1 to 10 foreach (seq.append(_))

    val filtered = seq filter (i => i % 2 == 0)
    val test: BufferSeq_Int = filtered

    assert(filtered.size == 5)
    2 to 10 by 2 foreach (i => assert(filtered((i - 1) / 2) == i))
  }

  test("foreachMacro") {
    val seq: Seq_Int = new BufferSeq_Int
    1 to 10 foreach (seq.append(_))

    var sum = 0
    seq foreachMacro (sum += _)
    assert(sum == 10 * 11 / 2)
  }

  test("foldLeft") {
    val seq: Seq_Int = new BufferSeq_Int
    1 to 3 foreach (seq.append(_))

    assert(seq.foldLeft(0)((acc, el) => (acc + el) * el) == 27)
    assert(new BufferSeq_Int().foldLeft[Int](3)((_, _) => 1) == 3)
  }

  test("foldRight") {
    val seq: Seq_Int = new BufferSeq_Int
    1 to 3 foreach (seq.append(_))

    assert(seq.foldRight(0)((el, acc) => (acc + el) * el) == 23)
    assert(new BufferSeq_Int().foldRight[Int](3)((_, _) => 1) == 3)
  }

  test("reduceLeft") {
    val seq: Seq_Int = new BufferSeq_Int
    1 to 3 foreach (seq.append(_))

    assert(seq.reduceLeft((acc, el) => (acc + el) * el) == 27)
  }

  test("reduceRight") {
    val seq: Seq_Int = new BufferSeq_Int
    1 to 3 foreach (seq.append(_))

    assert(seq.reduceRight((el, acc) => (acc + el) * el) == 11)
  }

  test("transform") {
    val seq: Seq_Int = new BufferSeq_Int
    1 to 10 foreach (seq.append(_))

    seq.transform(_ * 10)

    1 to 10 foreach (i => assert(seq(i - 1) == i * 10))
  }

  test("forall") {
    val seq: Seq_Int = new BufferSeq_Int
    2 to 100 by 2 foreach (seq.append(_))

    assert(seq.forall(_ % 2 == 0))
    assert(!seq.forall(_ > 50))
  }

  test("exists") {
    val seq: Seq_Int = new BufferSeq_Int
    2 to 100 by 2 foreach (seq.append(_))

    assert(seq.exists(_ == 62))
    assert(!seq.exists(_ % 2 == 1))
  }

  test("sameElements") {
    val seq: Seq_Int = new BufferSeq_Int
    1 to 10 foreach (seq.append(_))

    val same: Seq_Int = new BufferSeq_Int
    1 to 10 foreach (same.append(_))
    assert(seq sameElements same)

    val different: Seq_Int = new BufferSeq_Int
    1 to 10 foreach (i => different.append(i * i))
    assert(!seq.sameElements(different))

    val smaller: Seq_Int = new BufferSeq_Int
    1 to 5 foreach (smaller.append(_))
    assert(!seq.sameElements(smaller))
    assert(!smaller.sameElements(seq))
  }
}

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
    set foreachMacro (sum += _)
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

class SpecializedMapTest extends FunSuite with BeforeAndAfter with MapTest {
  def provideMap_Int_Object: Map[Int, Object] = new HashMap_Int_Object
  def provideMap_Object_Int: Map[Object, Int] = new HashMap_Object_Int

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
    1 to 10 foreach (i => assert(mapped(i).get == (i * 10).toString))
  }

  test("mapValues (_, string) to (_, int)") {
    val map = new HashMap_Int_Object[String]
    1 to 10 foreach (i => map.put(i, (i * 10) toString))

    val mapped = map mapValues (Integer.parseInt(_))
    val test: Map_Int_Int = mapped

    assert(mapped.size == 10)
    1 to 10 foreach (i => assert(mapped(i).get == i * 10))
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
    2 to 10 by 2 foreach (i => assert(filtered(i).get == i.toString))
  }

  test("fold") {
    val map = new HashMap_Int_Object[String]
    1 to 5 foreach (i => map.put(i, i toString))

    val sum = map.fold(3) { (acc, k, v) =>
      acc + k + v.length
    }
    assert(sum == 3 + 5 * 6 / 2 + 5)

    assert(new HashMap_Short_Boolean().fold(7) { (acc, k, v) =>
      acc + k + (if (v) 1 else 0)
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
    assert(sum == "12345")
  }

  test("transformValues") {
    val map = new HashMap_Int_Int
    1 to 10 foreach (i => map.put(i, i))

    map.transformValues(_ * 10)

    1 to 10 foreach (i => assert(map(i).get == i * 10))
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
