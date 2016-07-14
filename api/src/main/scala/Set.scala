package offheap.collection

trait Set[A] extends Any with Traversable1[A] {
  def apply(elem: A): Boolean
  def add(elem: A): Boolean
  def remove(elem: A): Boolean
}

trait GenericSet[A] extends Any with Set[A] {
  def intersect(that: Set[A]): Set[A]
  def union(that: Set[A]): Set[A]
  def diff(that: Set[A]): Set[A]
}
