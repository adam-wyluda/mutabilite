package offheap.collection

trait Set[A] extends Traversable[A] {
  def apply(elem: A): Boolean
  def add(elem: A): Boolean
  def remove(elem: A): Boolean
  def intersect(that: Set[A]): Set[A]
  def union(that: Set[A]): Set[A]
  def diff(that: Set[A]): Set[A]
}