package test

import offheap.collection._
import org.scalatest.{BeforeAndAfter, FunSuite}

class NaiveSeqTest extends FunSuite with BeforeAndAfter with SeqTest {
  def provideSeq_Int: Seq[Int] = new NaiveSeq[Int]
}

class NaiveSetTest extends FunSuite with BeforeAndAfter with GenericSetTest {
  def provideSet_Int: GenericSet[Int] = new NaiveSet[Int]
}

class NaiveMapTest extends FunSuite with BeforeAndAfter with MapTest {
  def provideMap_Int_Object: Map[Int, Object] = new NaiveMap[Int, Object]
  def provideMap_Object_Int: Map[Object, Int] = new NaiveMap[Object, Int]
}
