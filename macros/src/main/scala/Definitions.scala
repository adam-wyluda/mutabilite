package offheap.collection.macros

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

  def maybeParametrized[T: WeakTypeTag](clazz: ClassSymbol): Type = {
    val tpe = wt[T]
    val prefixedTpe = clazz.toType.dealias
    if (tpe <:< AnyRefTpe) {
      prefixedTpe.substituteTypes(prefixedTpe.typeArgs.map(_ typeSymbol), List(tpe))
    } else prefixedTpe
  }

  def seqType[T: WeakTypeTag]: Type = maybeParametrized[T](SeqClass(typeName[T]))

  def bufferType[T: WeakTypeTag]: Type = maybeParametrized[T](BufferSeqClass(typeName[T]))

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
