package test

import scala.{collection => stdlib}
import org.scalatest.{BeforeAndAfter, FunSuite}
import offheap.collection._

class OptTest extends FunSuite {
  val opt = new Opt_Int(25)
  val none = None_Int

  test("isEmpty") {
    assert(opt.nonEmpty)
    assert(none.isEmpty)
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
