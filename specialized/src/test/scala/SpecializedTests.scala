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
    val seq = new BufferSeq_Int
    1 to 3 foreach (seq.append(_))

    val mapped = seq map { _ toString }
    val test: BufferSeq_Object[String] = mapped
    assert(mapped.size == 3)
    1 to 3 foreach (i => assert(mapped(i - 1) == i.toString))
  }

  test("map string to int") {
    val seq = new BufferSeq_Object[String]
    1 to 3 foreach (i => seq.append(i toString))

    val mapped = seq map { Integer.parseInt(_) }
    val test: BufferSeq_Int = mapped
    assert(mapped.size == 3)
    1 to 3 foreach (i => assert(mapped(i - 1) == i))
  }

  test("map int") {
    val seq = new BufferSeq_Int
    1 to 3 foreach (seq.append(_))

    val mapped = seq.map(i => i * 2)
    val test: BufferSeq_Int = mapped
    assert(mapped.size == 3)
    1 to 3 foreach (i => assert(mapped(i - 1) == i * 2))
  }

  test("flatMap int") {
    val seq = new BufferSeq_Int
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
    val seq = new BufferSeq_Int
    1 to 3 foreach (seq.append(_))

    val mapped = seq.map(i => i * 2.0f)
    val test: Seq_Float = mapped
    assert(mapped.size == 3)
    1 to 3 foreach (i => assert(mapped(i - 1) == i * 2.0f))
  }

  test("flatMap") {
    val seq = new BufferSeq_Int
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
}

class SpecializedSetTest extends FunSuite with BeforeAndAfter with SetTest {
  def provideSet_Int: Set[Int] = new HashSet_Int
}

class SpecializedMapTest extends FunSuite with BeforeAndAfter with MapTest {
  def provideMap_Int_Object: Map[Int, Object] = new HashMap_Int_Object
  def provideMap_Object_Int: Map[Object, Int] = new HashMap_Object_Int
}
