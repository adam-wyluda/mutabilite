package offheap.collection

import scala.language.experimental.{macros => CanMacro}

package object ops {

  implicit class SeqOps[A](val seq: Seq[A]) extends AnyVal {
    def map[B](f: A => B): Seq[B]              = macro offheap.collection.macros.SeqOpsMacros.map[B]
    def flatMap[B](f: A => Seq[B]): Seq[B]     = macro offheap.collection.macros.SeqOpsMacros.flatMap[B]
    def filter(f: A => Boolean): Seq[A]        = macro offheap.collection.macros.SeqOpsMacros.filter[A]
    def foreachMacro(f: A => Unit): Unit       = macro offheap.collection.macros.SeqOpsMacros.foreach
    def foldLeft[B](z: B)(op: (B, A) => B): B  = macro offheap.collection.macros.SeqOpsMacros.foldLeft[B]
    def foldRight[B](z: B)(op: (A, B) => B): B = macro offheap.collection.macros.SeqOpsMacros.foldRight[B]
    def reduceLeft(op: (A, A) => A): A         = macro offheap.collection.macros.SeqOpsMacros.reduceLeft[A]
    def reduceRight(op: (A, A) => A): A        = macro offheap.collection.macros.SeqOpsMacros.reduceRight[A]
    def transform(f: A => A): Unit             = macro offheap.collection.macros.SeqOpsMacros.transform
    def forall(p: A => Boolean): Boolean       = macro offheap.collection.macros.SeqOpsMacros.forall
    def exists(p: A => Boolean): Boolean       = macro offheap.collection.macros.SeqOpsMacros.exists
    def sameElements(other: Seq[A]): Boolean   = macro offheap.collection.macros.SeqOpsMacros.sameElements
    def zipToMap[B](values: Seq[B]): Map[A, B] = macro offheap.collection.macros.SeqOpsMacros.zipToMap[A, B]
  }

  implicit class SetOps[A](val set: Set[A]) extends AnyVal {
    def map[B](f: A => B): Set[B]          = macro offheap.collection.macros.SetOpsMacros.map[B]
    def flatMap[B](f: A => Set[B]): Set[B] = macro offheap.collection.macros.SetOpsMacros.flatMap[B]
    def filter(f: A => Boolean): Set[A]    = macro offheap.collection.macros.SetOpsMacros.filter[A]
    def foreachMacro(f: A => Unit): Unit   = macro offheap.collection.macros.SetOpsMacros.foreach
    def fold[B](z: B)(op: (B, A) => B): B  = macro offheap.collection.macros.SetOpsMacros.fold[B]
    def reduce(op: (A, A) => A): A         = macro offheap.collection.macros.SetOpsMacros.reduce[A]
    def forall(p: A => Boolean): Boolean   = macro offheap.collection.macros.SetOpsMacros.forall
    def exists(p: A => Boolean): Boolean   = macro offheap.collection.macros.SetOpsMacros.exists
  }

  implicit class MapOps[K, V](val map: Map[K, V]) extends AnyVal {
    def map[B](f: (K, V) => B): Seq[B]          = macro offheap.collection.macros.MapOpsMacros.map[B]
    def mapKeys[B](f: K => B): Map[B, V]        = macro offheap.collection.macros.MapOpsMacros.mapKeys[V, B]
    def mapValues[B](f: V => B): Map[K, B]      = macro offheap.collection.macros.MapOpsMacros.mapValues[K, B]
    def flatMap[B](f: (K, V) => Seq[B]): Seq[B] = macro offheap.collection.macros.MapOpsMacros.flatMap[B]
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
