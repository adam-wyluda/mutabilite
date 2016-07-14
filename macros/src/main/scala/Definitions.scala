package offheap.collection.macros

import offheap.collection._

import scala.reflect.macros.whitebox

trait Definitions {

  val c: whitebox.Context

  import c.universe.{weakTypeOf => wt, _}
  import c.universe.definitions._
  import c.universe.rootMirror._

  def SeqClass(elemType: String) =
    staticClass("offheap.collection.Seq_" + elemType)

  def BufferSeqClass(elemType: String) =
    staticClass("offheap.collection.BufferSeq_" + elemType)

  def SetClass(elemType: String) =
    staticClass("offheap.collection.Set_" + elemType)

  def HashSetClass(elemType: String) =
    staticClass("offheap.collection.HashSet_" + elemType)

  def seqType[T: WeakTypeTag]: Type =
    if (wt[T] <:< AnyRefTpe) {
      wt[Seq_Object[T]]
    } else SeqClass(typeName[T]).toType

  def bufferType[T: WeakTypeTag]: Type =
    if (wt[T] <:< AnyRefTpe) {
      wt[BufferSeq_Object[T]]
    } else BufferSeqClass(typeName[T]).toType

  def setType[T: WeakTypeTag]: Type =
    if (wt[T] <:< AnyRefTpe) {
      wt[Set_Object[T]]
    } else SetClass(typeName[T]).toType

  def hashSetType[T: WeakTypeTag]: Type =
    if (wt[T] <:< AnyRefTpe) {
      wt[HashSet_Object[T]]
    } else HashSetClass(typeName[T]).toType

  def typeName[B: WeakTypeTag]: String =
    wt[B] match {
      case BooleanTpe => "Boolean"
      case CharTpe => "Char"
      case ByteTpe => "Byte"
      case ShortTpe => "Short"
      case IntTpe => "Int"
      case LongTpe => "Long"
      case FloatTpe => "Float"
      case DoubleTpe => "Double"
      case _ => "Object"
    }
}
