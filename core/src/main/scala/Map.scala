package offheap.collection

trait Map[K, V] extends Traversable2[K, V] {
  def apply(key: K): V
  def get(key: K): Option[V]
  def put(key: K, value: V): Unit
  def remove(key: K): Unit
  def keys: Set[K]
  def values: Seq[V]
  def contains(key: K): Boolean
  def compact: Unit
  def capacity: Int
}
