package offheap.collection

sealed trait Traversable {
  def size: Int
  def isEmpty: Boolean
  def nonEmpty: Boolean = !isEmpty
}

trait Traversable1[A] extends Traversable {
  def foreach(f: A => Unit): Unit
}

trait Traversable2[A, B] extends Traversable {
  def foreach(f: (A, B) => Unit): Unit
}
