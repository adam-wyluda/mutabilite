package offheap

import scala.language.experimental.{macros => CanMacro}

package object collection {
  implicit class SeqOps[A](val seq: Seq[A]) extends AnyVal {
    def map[B](f: (A) => B): Seq[B] = macro offheap.collection.macros.SeqOpsMacros.map[B]
    def flatMap[B](f: (A) => Seq[B]): Seq[B] = macro offheap.collection.macros.SeqOpsMacros.flatMap[B]
    def filter(f: (A) => Boolean): Seq[A] = macro offheap.collection.macros.SeqOpsMacros.filter
  }
}
