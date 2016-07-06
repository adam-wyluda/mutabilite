package offheap

import scala.language.experimental.macros

package object collection {
  implicit class SpecializedSeqOps[A](val seq: Seq[A]) extends AnyVal with SeqOps[A] {
    def map[B](f: (A) => B): Seq[B] = macro offheap.collection.macros.SeqOpsMacros.map
    def flatMap[B](f: (A) => Seq[B]): Seq[B] = macro offheap.collection.macros.SeqOpsMacros.flatMap
    def filter(f: (A) => Boolean): Seq[A] = macro offheap.collection.macros.SeqOpsMacros.filter
  }
}
