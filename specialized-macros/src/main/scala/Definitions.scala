package offheap.collection.macros

import offheap.collection._

import scala.reflect.macros.whitebox

trait Definitions {

  val c: whitebox.Context

  import c.universe.{weakTypeOf => wt, _}
  import c.universe.definitions._
  import c.universe.rootMirror._

  val UnsupportedOperationExceptionClass = staticClass(
      "java.lang.UnsupportedOperationException")

  def SeqClass(elemType: String) =
    staticClass("offheap.collection.Seq_" + elemType)

  def BufferSeqClass(elemType: String) =
    staticClass("offheap.collection.BufferSeq_" + elemType)

  def SetClass(elemType: String) =
    staticClass("offheap.collection.Set_" + elemType)

  def HashSetClass(elemType: String) =
    staticClass("offheap.collection.HashSet_" + elemType)

  def MapClass(keyType: String, valueType: String) =
    staticClass("offheap.collection.Map_" + keyType + "_" + valueType)

  def HashMapClass(keyType: String, valueType: String) =
    staticClass("offheap.collection.HashMap_" + keyType + "_" + valueType)

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

  def mapType[K: WeakTypeTag, V: WeakTypeTag]: Type =
    if (wt[K] <:< AnyRefTpe && wt[V] <:< AnyRefTpe) wt[Map_Object_Object[K, V]]
    else if (wt[K] <:< AnyRefTpe) map_object_v[K, V]
    else if (wt[V] <:< AnyRefTpe) map_k_object[K, V]
    else MapClass(typeName[K], typeName[V]).toType

  def map_object_v[K: WeakTypeTag, V: WeakTypeTag]: Type =
    wt[V] match {
      case BooleanTpe => wt[Map_Object_Boolean[K]]
      case CharTpe => wt[Map_Object_Char[K]]
      case ByteTpe => wt[Map_Object_Byte[K]]
      case ShortTpe => wt[Map_Object_Short[K]]
      case IntTpe => wt[Map_Object_Int[K]]
      case LongTpe => wt[Map_Object_Long[K]]
      case FloatTpe => wt[Map_Object_Float[K]]
      case DoubleTpe => wt[Map_Object_Double[K]]
    }

  def map_k_object[K: WeakTypeTag, V: WeakTypeTag]: Type =
    wt[K] match {
      case BooleanTpe => wt[Map_Boolean_Object[V]]
      case CharTpe => wt[Map_Char_Object[V]]
      case ByteTpe => wt[Map_Byte_Object[V]]
      case ShortTpe => wt[Map_Short_Object[V]]
      case IntTpe => wt[Map_Int_Object[V]]
      case LongTpe => wt[Map_Long_Object[V]]
      case FloatTpe => wt[Map_Float_Object[V]]
      case DoubleTpe => wt[Map_Double_Object[V]]
    }

  def hashMapType[K: WeakTypeTag, V: WeakTypeTag]: Type =
    if (wt[K] <:< AnyRefTpe && wt[V] <:< AnyRefTpe)
      wt[HashMap_Object_Object[K, V]]
    else if (wt[K] <:< AnyRefTpe) hashMap_object_v[K, V]
    else if (wt[V] <:< AnyRefTpe) hashMap_k_object[K, V]
    else HashMapClass(typeName[K], typeName[V]).toType

  def hashMap_object_v[K: WeakTypeTag, V: WeakTypeTag]: Type =
    wt[V] match {
      case BooleanTpe => wt[HashMap_Object_Boolean[K]]
      case CharTpe => wt[HashMap_Object_Char[K]]
      case ByteTpe => wt[HashMap_Object_Byte[K]]
      case ShortTpe => wt[HashMap_Object_Short[K]]
      case IntTpe => wt[HashMap_Object_Int[K]]
      case LongTpe => wt[HashMap_Object_Long[K]]
      case FloatTpe => wt[HashMap_Object_Float[K]]
      case DoubleTpe => wt[HashMap_Object_Double[K]]
    }

  def hashMap_k_object[K: WeakTypeTag, V: WeakTypeTag]: Type =
    wt[K] match {
      case BooleanTpe => wt[HashMap_Boolean_Object[V]]
      case CharTpe => wt[HashMap_Char_Object[V]]
      case ByteTpe => wt[HashMap_Byte_Object[V]]
      case ShortTpe => wt[HashMap_Short_Object[V]]
      case IntTpe => wt[HashMap_Int_Object[V]]
      case LongTpe => wt[HashMap_Long_Object[V]]
      case FloatTpe => wt[HashMap_Float_Object[V]]
      case DoubleTpe => wt[HashMap_Double_Object[V]]
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
