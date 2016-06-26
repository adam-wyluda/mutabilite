package offheap.collection

sealed trait Traversable extends Any {
  def size: Int
  def isEmpty: Boolean
  def nonEmpty: Boolean = !isEmpty
}

trait Traversable1[A] extends Any with Traversable {
  def foreach(f: A => Unit): Unit
}

trait Traversable2[A, B] extends Any with Traversable {
  def foreach(f: (A, B) => Unit): Unit
}
