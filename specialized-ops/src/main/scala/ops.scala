package offheap.collection

import scala.language.experimental.{macros => CanMacro}

package object ops {

  implicit class SeqOps[T](val seq: Seq[T]) extends AnyVal {
    def map[B](f: T => B): Seq[B]              = macro offheap.collection.macros.SeqOpsMacros.map[B]
    def flatMap[B](f: T => Seq[B]): Seq[B]     = macro offheap.collection.macros.SeqOpsMacros.flatMap[B]
    def filter(f: T => Boolean): Seq[T]        = macro offheap.collection.macros.SeqOpsMacros.filter[T]
    def foreachMacro(f: T => Unit): Unit       = macro offheap.collection.macros.SeqOpsMacros.foreach
    def foldLeft[B](z: B)(op: (B, T) => B): B  = macro offheap.collection.macros.SeqOpsMacros.foldLeft[B]
    def foldRight[B](z: B)(op: (T, B) => B): B = macro offheap.collection.macros.SeqOpsMacros.foldRight[B]
    def reduceLeft(op: (T, T) => T): T         = macro offheap.collection.macros.SeqOpsMacros.reduceLeft[T]
    def reduceRight(op: (T, T) => T): T        = macro offheap.collection.macros.SeqOpsMacros.reduceRight[T]
    def transform(f: T => T): Unit             = macro offheap.collection.macros.SeqOpsMacros.transform
    def forall(p: T => Boolean): Boolean       = macro offheap.collection.macros.SeqOpsMacros.forall
    def exists(p: T => Boolean): Boolean       = macro offheap.collection.macros.SeqOpsMacros.exists
    def sameElements(other: Seq[T]): Boolean   = macro offheap.collection.macros.SeqOpsMacros.sameElements
    def zipToMap[B](values: Seq[B]): Map[T, B] = macro offheap.collection.macros.SeqOpsMacros.zipToMap[T, B]
  }

  implicit class SetOps[T](val set: Set[T]) extends AnyVal {
    def map[B](f: T => B): Set[B]          = macro offheap.collection.macros.SetOpsMacros.map[B]
    def flatMap[B](f: T => Set[B]): Set[B] = macro offheap.collection.macros.SetOpsMacros.flatMap[B]
    def filter(f: T => Boolean): Set[T]    = macro offheap.collection.macros.SetOpsMacros.filter[T]
    def foreachMacro(f: T => Unit): Unit   = macro offheap.collection.macros.SetOpsMacros.foreach
    def fold[B](z: B)(op: (B, T) => B): B  = macro offheap.collection.macros.SetOpsMacros.fold[B]
    def reduce(op: (T, T) => T): T         = macro offheap.collection.macros.SetOpsMacros.reduce[T]
    def forall(p: T => Boolean): Boolean   = macro offheap.collection.macros.SetOpsMacros.forall
    def exists(p: T => Boolean): Boolean   = macro offheap.collection.macros.SetOpsMacros.exists
  }

  implicit class MapOps[K, V](val map: Map[K, V]) extends AnyVal {
    def map[T](f: (K, V) => T): Seq[T]          = macro offheap.collection.macros.MapOpsMacros.map[T]
    def mapKeys[T](f: K => T): Map[T, V]        = macro offheap.collection.macros.MapOpsMacros.mapKeys[V, T]
    def mapValues[T](f: V => T): Map[K, T]      = macro offheap.collection.macros.MapOpsMacros.mapValues[K, T]
    def flatMap[T](f: (K, V) => Seq[T]): Seq[T] = macro offheap.collection.macros.MapOpsMacros.flatMap[T]
    def filter(f: (K, V) => Boolean): Map[K, V] = macro offheap.collection.macros.MapOpsMacros.filter[K, V]
    def foreachMacro(f: (K, V) => Unit): Unit   = macro offheap.collection.macros.MapOpsMacros.foreach
    def fold[B](z: B)(op: (B, K, V) => B): B    = macro offheap.collection.macros.MapOpsMacros.fold[B]
    def reduceKeys(op: (K, K) => K): K          = macro offheap.collection.macros.MapOpsMacros.reduceKeys[K]
    def reduceValues(op: (V, V) => V): V        = macro offheap.collection.macros.MapOpsMacros.reduceValues[V]
    def transformValues(f: V => V): Unit        = macro offheap.collection.macros.MapOpsMacros.transformValues
    def forall(p: (K, V) => Boolean): Boolean   = macro offheap.collection.macros.MapOpsMacros.forall
    def exists(p: (K, V) => Boolean): Boolean   = macro offheap.collection.macros.MapOpsMacros.exists
  }
}
