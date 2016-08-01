package offheap.collection

import scala.reflect.ClassTag

class MapSet[A](initialSize: Int = 8)(implicit tag: ClassTag[A])
    extends Set[A] {

  private[this] val map = new HashMap[A, Unit](initialSize)

  def apply(elem: A): Boolean = map contains elem

  def add(elem: A): Boolean = map.put(elem, ()).empty

  def remove(elem: A): Boolean = map.remove(elem).notEmpty

  def intersect(that: Set[A]): Set[A] = {
    val result = new MapSet[A]
    foreach { a =>
      if (that(a)) result.add(a)
    }
    result
  }

  def union(that: Set[A]): Set[A] = {
    val result = new MapSet[A]
    this foreach (result.add(_))
    that foreach (result.add(_))
    result
  }

  def diff(that: Set[A]): Set[A] = {
    val result = new MapSet[A]
    foreach { a =>
      if (!that(a)) result.add(a)
    }
    result
  }

  override def empty = map.empty
  override def size: Int = map.size
  override def capacity: Int = map.capacity
  override def foreach(f: A => Unit): Unit = map foreach ((k, _) => f(k))

  def compact = ???
}
