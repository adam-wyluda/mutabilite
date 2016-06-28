package test

import offheap.collection._
import org.scalatest.{BeforeAndAfter, FunSuite}

import HashEq.Implicits._

class OffheapSetTest extends FunSuite with BeforeAndAfter {//with SetTest {
def provideSet_Int: Set[Int] = new OffheapHashSet_Int
}
