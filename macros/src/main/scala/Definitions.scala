package offheap.collection.macros

import scala.reflect.macros.whitebox

trait Definitions {

  val c: whitebox.Context

  import c.universe._
  import c.universe.definitions._
  import c.universe.rootMirror._

//  val SeqOpsClass = staticClass("offheap.collection.SeqOps")
}
