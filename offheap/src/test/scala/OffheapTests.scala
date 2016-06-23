package test

import offheap.collection._
import org.scalatest.{BeforeAndAfter, FunSuite}

import HashEq.Implicits._
import SeqBuilders._

class OffheapOptTest extends FunSuite with BeforeAndAfter with OptTest {
  def provideOpt_Int(value: Int): Opt[Int] = new OffheapOpt_Int(value)
  def provideNone: Opt[Int] = OffheapNone_Int
}

class OffheapSeqTest extends FunSuite with BeforeAndAfter with SeqTest {
  def provideSeq_Int: Seq[Int] = new OffheapBufferSeq_Int

  test("map_Int") {
    val seq: OffheapBufferSeq_Int = new OffheapBufferSeq_Int
    1 to 3 foreach (seq.append(_))

    val mapped: OffheapBufferSeq_Int = seq map_Int (_ * 2)
    assert(mapped.size == 3)
    1 to 3 foreach (i => assert(mapped(i - 1) == i * 2))
  }

  test("flatMap_Int") {
    val seq: OffheapBufferSeq_Int = new OffheapBufferSeq_Int
    1 to 5 by 2 foreach (seq.append(_))

    val mapped: OffheapBufferSeq_Int = seq flatMap_Int { i =>
      val r = new OffheapBufferSeq_Int
      r.append(i)
      r.append(i + 1)
      r
    }
    assert(mapped.size == 6)
    1 to 6 foreach (i => assert(mapped(i - 1) == i))
  }

  test("filter") {
    val seq: Seq_Int = new OffheapBufferSeq_Int
    1 to 10 foreach (seq.append(_))

    val filtered = seq filter (i => i % 2 == 0)
    val test: Seq_Int = filtered

    assert(filtered.size == 5)
    2 to 10 by 2 foreach (i => assert(filtered((i - 1) / 2) == i))
  }
}

class OffheapSetTest extends FunSuite with BeforeAndAfter with SetTest {
  def provideSet_Int: Set[Int] = new OffheapHashSet_Int
}

class OffheapMapTest extends FunSuite with BeforeAndAfter with MapTest {
  def provideMap_Int_Object: Map[Int, Object] = new OffheapHashMap_Int_Object
  def provideMap_Object_Int: Map[Object, Int] = new OffheapHashMap_Object_Int
}
