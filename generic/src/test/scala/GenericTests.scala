package test

import offheap.collection._
import org.scalatest.{BeforeAndAfter, FunSuite}

class BufferSeqTest extends FunSuite with BeforeAndAfter with SeqTest {
  def provideSeq_Int: Seq[Int] = new BufferSeq[Int]
}

class HashSetTest extends FunSuite with BeforeAndAfter with GenericSetTest {
  def provideSet_Int: GenericSet[Int] = new HashSet[Int]
}

class HashMapTest extends FunSuite with BeforeAndAfter with MapTest {
  def provideMap_Int_Object: Map[Int, Object] = new HashMap[Int, Object]
  def provideMap_Object_Int: Map[Object, Int] = new HashMap[Object, Int]
}
