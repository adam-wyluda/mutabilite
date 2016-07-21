package test

import offheap.collection._
import org.scalatest.{BeforeAndAfter, FunSuite}

import HashEq.Implicits._

class SpecializedMapTest extends FunSuite with BeforeAndAfter with MapTest {
  def provideMap_Int_Object: Map[Int, Object] = new HashMap_Int_Object
  def provideMap_Object_Int: Map[Object, Int] = new HashMap_Object_Int
}
