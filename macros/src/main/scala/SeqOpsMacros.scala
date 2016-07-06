package offheap.collection.macros

import scala.reflect.macros.blackbox

class SeqOpsMacros(val c: blackbox.Context) {
  import c.universe._
  import c.universe.definitions._

  def map = q""
  def flatMap = q""
  def filter = q""
}
