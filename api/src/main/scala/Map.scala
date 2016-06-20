package offheap.collection

trait Map[K, V] extends Traversable2[K, V] {
  def apply(key: K): Opt[V]
  def put(key: K, value: V): Opt[V]
  def remove(key: K): Opt[V]
  def keys: Set[K]
  def values: Seq[V]
  def contains(key: K): Boolean
}
