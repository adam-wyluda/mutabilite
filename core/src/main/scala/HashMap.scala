package offheap.collection

class HashMap[K, V]
  extends Map[K, V] {

  def apply(key: K): Opt[V] = ???

  def put(key: K, value: V): Opt[V] = ???

  def remove(key: K): Opt[V] = ???

  def keys: NaiveSet[K] = ???

  def values: NaiveSeq[V] = ???

  def contains(key: K): Boolean = ???

  private def keyIndex(key: K) = ???

  override def isEmpty = ???

  override def size: Int = ???

  override def foreach[U](f: ((K, V)) => U): Unit = ???
}
