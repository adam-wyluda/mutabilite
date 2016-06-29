package test

import offheap.collection._
import org.scalatest.{BeforeAndAfter, FunSuite}

class OffheapOptTest extends FunSuite with BeforeAndAfter {
  implicit val alloc = scala.offheap.malloc

  val optInt = OffheapOpt_Int(25)
  val noneInt = OffheapOpt_Int.empty

  test("isEmpty") {
    assert(optInt.notEmpty)
    assert(noneInt.empty)
  }

  test("get") {
    assert(optInt.get == 25)
    intercept[NullPointerException] {
      noneInt.get
    }
  }

  test("foreach") {
    var optSum = 0
    var noneSum = 0

    optInt.foreach(optSum += _)
    noneInt.foreach(noneSum += _)

    assert(optSum == 25)
    assert(noneSum == 0)
  }
}
