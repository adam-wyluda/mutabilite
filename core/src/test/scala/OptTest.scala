package test

import offheap.collection._
import org.scalatest.{BeforeAndAfter, FunSuite}

import HashEq.Implicits._

class OptTest extends FunSuite with BeforeAndAfter {
  val opt = new Some_Int(25)
  val none = None_Int

  test("isEmpty") {
    assert(opt.notEmpty)
    assert(none.empty)
  }

  test("get") {
    assert(opt.get == 25)
    intercept[NoSuchElementException] {
      none.get
    }
  }

  test("foreach") {
    var optSum = 0
    var noneSum = 0

    opt.foreach(optSum += _)
    none.foreach(noneSum += _)

    assert(optSum == 25)
    assert(noneSum == 0)
  }
}
