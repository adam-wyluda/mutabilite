package test

import offheap.collection._
import org.scalatest.{BeforeAndAfter, FunSuite}

import HashEq.Implicits._

class OffheapOptTest extends FunSuite with BeforeAndAfter {//with OptTest {
implicit val alloc = scala.offheap.malloc

  def provideOpt_Int(value: Int): Opt_Int = OffheapOpt_Int(value)
  def provideNone: Opt_Int = OffheapOpt_Int.empty
}
