package mutabilite.macros

import mutabilite._

import scala.reflect.macros.whitebox

trait Definitions {

  val c: whitebox.Context

  import c.universe.{weakTypeOf => wt, _}
  import c.universe.definitions._
  import c.universe.rootMirror._

  val UnsupportedOperationExceptionClass = staticClass(
      "java.lang.UnsupportedOperationException")

  def SeqClass(elemType: String) =
    staticClass("mutabilite.Seq_" + elemType)

  def BufferSeqClass(elemType: String) =
    staticClass("mutabilite.BufferSeq_" + elemType)

  def SetClass(elemType: String) =
    staticClass("mutabilite.Set_" + elemType)

  def HashSetClass(elemType: String) =
    staticClass("mutabilite.HashSet_" + elemType)

  def MapClass(keyType: String, valueType: String) =
    staticClass("mutabilite.Map_" + keyType + "_" + valueType)

  def HashMapClass(keyType: String, valueType: String) =
    staticClass("mutabilite.HashMap_" + keyType + "_" + valueType)

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

  def hashMapTypeSeq[K: WeakTypeTag, SV: WeakTypeTag]: Type =
    wt[SV] match {
      case BooleanTpe => hashMapType[K, Seq_Boolean]
      case CharTpe => hashMapType[K, Seq_Char]
      case ByteTpe => hashMapType[K, Seq_Byte]
      case ShortTpe => hashMapType[K, Seq_Short]
      case IntTpe => hashMapType[K, Seq_Int]
      case LongTpe => hashMapType[K, Seq_Long]
      case FloatTpe => hashMapType[K, Seq_Float]
      case DoubleTpe => hashMapType[K, Seq_Double]
      case _ => hashMapType[K, Seq_Object[SV]]
    }

  def hashMapType[K: WeakTypeTag, V: WeakTypeTag]: Type =
    if (wt[K] <:< AnyRefTpe && wt[V] <:< AnyRefTpe)
      wt[HashMap_Object_Object[K, V]]
    else if (wt[K] <:< AnyRefTpe) hashMap_object_v[K, V]
    else if (wt[V] <:< AnyRefTpe) hashMap_k_object[K, V]
    else HashMapClass(typeName[K], typeName[V]).toType

  def hashMap_object_v[K: WeakTypeTag, V: WeakTypeTag]: Type =
    wt[V] match {
      case CharTpe => wt[HashMap_Object_Char[K]]
      case ShortTpe => wt[HashMap_Object_Short[K]]
      case IntTpe => wt[HashMap_Object_Int[K]]
      case LongTpe => wt[HashMap_Object_Long[K]]
      case FloatTpe => wt[HashMap_Object_Float[K]]
      case DoubleTpe => wt[HashMap_Object_Double[K]]
    }

  def hashMap_k_object[K: WeakTypeTag, V: WeakTypeTag]: Type =
    wt[K] match {
      case IntTpe => wt[HashMap_Int_Object[V]]
      case LongTpe => wt[HashMap_Long_Object[V]]
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
