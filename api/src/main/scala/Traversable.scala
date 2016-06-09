package offheap.collection

trait Traversable[A] extends Any {
  def size: Int
  def isEmpty: Boolean
  def nonEmpty: Boolean = !isEmpty
  def foreach[U](f: A => U)
}
