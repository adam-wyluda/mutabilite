package test

import offheap.collection._
import org.scalatest.{BeforeAndAfter, FunSuite}

class GenericSeqTest extends FunSuite with BeforeAndAfter with SeqTest {
  def provideSeq_Int: Seq[Int] = new BufferSeq[Int]
}

class GenericSetTest extends FunSuite with BeforeAndAfter with SetTest {
  def provideSet_Int: Set[Int] = new HashSet[Int]
}

class GenericMapTest extends FunSuite with BeforeAndAfter with MapTest {
  def provideMap_Int_Object: Map[Int, Object] = new HashMap[Int, Object]
  def provideMap_Object_Int: Map[Object, Int] = new HashMap[Object, Int]
}
