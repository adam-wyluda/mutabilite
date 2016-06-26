package test

import offheap.collection._
import org.scalatest.{BeforeAndAfter, FunSuite}

trait OptTest { this: FunSuite with BeforeAndAfter =>

  def provideOpt_Int(value: Int): Opt[Int]
  def provideNone: Opt[Int]

  val opt = provideOpt_Int(25)
  val none = provideNone

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
