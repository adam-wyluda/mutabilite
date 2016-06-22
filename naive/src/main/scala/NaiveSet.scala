package offheap.collection

import scala.reflect.ClassTag

class NaiveSet[A](implicit tag: ClassTag[A]) extends Set[A] {

  private[this] val seq = new NaiveSeq[A]

  def apply(elem: A): Boolean = seq.index(elem) != -1

  def add(elem: A): Boolean = {
    if (this(elem)) {
      false
    } else {
      seq.append(elem)
      true
    }
  }

  def remove(elem: A): Boolean = {
    if (this(elem)) {
      seq.remove(seq.index(elem))
      true
    } else {
      false
    }
  }

  def intersect(that: Set[A]): NaiveSet[A] = {
    val result = new NaiveSet[A]
    foreach { a =>
      if (that(a)) result.add(a)
    }
    result
  }

  def union(that: Set[A]): NaiveSet[A] = {
    val result = new NaiveSet[A]
    this foreach (result.add(_))
    that foreach (result.add(_))
    result
  }

  def diff(that: Set[A]): NaiveSet[A] = {
    val result = new NaiveSet[A]
    foreach { a =>
      if (!that(a)) result.add(a)
    }
    result
  }

  override def isEmpty = seq.isEmpty
  override def size: Int = seq.size
  override def foreach(f: A => Unit): Unit = seq foreach f
}
