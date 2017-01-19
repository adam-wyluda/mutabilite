package mutabilite.macros

import mutabilite._

import scala.reflect.macros.blackbox

trait Definitions {

  val c: blackbox.Context

  import c.universe.{weakTypeOf => wt, _}
  import c.universe.definitions._
  import c.universe.rootMirror._

  val UnsupportedOperationExceptionClass = staticClass(
      "java.lang.UnsupportedOperationException")

  def SeqClass(elemType: String) =
    staticClass("mutabilite.Seq_" + elemType)

  def SetClass(elemType: String) =
    staticClass("mutabilite.Set_" + elemType)

  def MapClass(keyType: String, valueType: String) =
    staticClass("mutabilite.Map_" + keyType + "_" + valueType)

  def seqType[T: WeakTypeTag]: Type =
    if (wt[T] <:< AnyRefTpe) {
      wt[Seq_Object[T]]
    } else SeqClass(typeName[T]).toType

  def setType[T: WeakTypeTag]: Type =
    if (wt[T] <:< AnyRefTpe) {
      wt[Set_Object[T]]
    } else SetClass(typeName[T]).toType

  def mapTypeSeq[K: WeakTypeTag, SV: WeakTypeTag]: Type =
    wt[SV] match {
      case BooleanTpe => mapType[K, Seq_Boolean]
      case CharTpe => mapType[K, Seq_Char]
      case ByteTpe => mapType[K, Seq_Byte]
      case ShortTpe => mapType[K, Seq_Short]
      case IntTpe => mapType[K, Seq_Int]
      case LongTpe => mapType[K, Seq_Long]
      case FloatTpe => mapType[K, Seq_Float]
      case DoubleTpe => mapType[K, Seq_Double]
      case _ => mapType[K, Seq_Object[SV]]
    }

  def mapType[K: WeakTypeTag, V: WeakTypeTag]: Type =
    if (wt[K] <:< AnyRefTpe && wt[V] <:< AnyRefTpe) wt[Map_Object_Object[K, V]]
    else if (wt[K] <:< AnyRefTpe) map_object_v[K, V]
    else if (wt[V] <:< AnyRefTpe) map_k_object[K, V]
    else MapClass(typeName[K], typeName[V]).toType

  def map_object_v[K: WeakTypeTag, V: WeakTypeTag]: Type =
    wt[V] match {
      case CharTpe => wt[Map_Object_Char[K]]
      case ShortTpe => wt[Map_Object_Short[K]]
      case IntTpe => wt[Map_Object_Int[K]]
      case LongTpe => wt[Map_Object_Long[K]]
      case FloatTpe => wt[Map_Object_Float[K]]
      case DoubleTpe => wt[Map_Object_Double[K]]
    }

  def map_k_object[K: WeakTypeTag, V: WeakTypeTag]: Type =
    wt[K] match {
      case IntTpe => wt[Map_Int_Object[V]]
      case LongTpe => wt[Map_Long_Object[V]]
    }

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
