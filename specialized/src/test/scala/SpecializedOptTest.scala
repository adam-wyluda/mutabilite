package test

import offheap.collection._
import org.scalatest.{BeforeAndAfter, FunSuite}

import HashEq.Implicits._

class SpecializedOptTest extends FunSuite with BeforeAndAfter with OptTest {
  def provideOpt_Int(value: Int): Opt[Int] = new Some_Int(value)
  def provideNone: Opt[Int] = None_Int
}
