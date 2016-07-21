package test

import offheap.collection._
import org.scalatest.{BeforeAndAfter, FunSuite}

import HashEq.Implicits._

class SpecializedSeqTest extends FunSuite with BeforeAndAfter with SeqTest {
  def provideSeq_Int: Seq[Int] = new BufferSeq_Int
}
