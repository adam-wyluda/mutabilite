package offheap.collection

trait Set[A] extends Any with Traversable1[A] {
  def apply(elem: A): Boolean
  def add(elem: A): Boolean
  def remove(elem: A): Boolean
}
