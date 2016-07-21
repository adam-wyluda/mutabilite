package test

import offheap.collection._
import org.scalatest.{BeforeAndAfter, FunSuite}

import HashEq.Implicits._

class SpecializedMapTest extends FunSuite with BeforeAndAfter with MapTest {
  def provideMap_Int_Object: Map[Int, Object] = new HashMap_Int_Object
  def provideMap_Object_Int: Map[Object, Int] = new HashMap_Object_Int

  test("compact") {
    val map = new HashMap_Int_Int
    1 to 50 foreach (i => map.put(i, i * i))
    assert(map.capacity == 64)

    1 to 25 foreach (map.remove(_))
    assert(map.capacity == 64)
    map.compact
    assert(map.capacity == 32)

    25 to 40 foreach (map.remove(_))
    assert(map.capacity == 32)
    map.compact
    assert(map.capacity == 16)

    assert(map.size == 10)
    41 to 50 foreach (i => assert(map(i).get == i * i))
  }
}
