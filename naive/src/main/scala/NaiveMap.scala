package offheap.collection

import scala.reflect.ClassTag

class NaiveMap[K, V](implicit tag: ClassTag[(K, V)],
                     tagKeys: ClassTag[K],
                     tagValues: ClassTag[V])
    extends Map[K, V] {

  private[this] val seq = new NaiveSeq[(K, V)]

  def apply(key: K): Opt[V] = {
    var result: Opt[V] = new None[V]
    seq foreach {
      case (k, v) =>
        if (k == key) result = new Some(v)
    }
    result
  }

  def put(key: K, value: V): Opt[V] = {
    val previous = remove(key)
    seq.append((key, value))
    previous
  }

  def remove(key: K): Opt[V] = {
    val previous = this(key)
    if (contains(key)) {
      val index = keyIndex(key)
      seq.remove(index)
    }
    previous
  }

  def keys: NaiveSet[K] = {
    val result = new NaiveSet[K]
    foreach { case (k, v) => result.add(k) }
    result
  }

  def values: NaiveSeq[V] = {
    val result = new NaiveSeq[V]
    foreach { case (k, v) => result.append(v) }
    result
  }

  def contains(key: K): Boolean = this(key).nonEmpty

  private def keyIndex(key: K) = {
    var result = -1
    var index = 0
    seq foreach { case (k, v) => if (k == key) result = index else index += 1 }
    result
  }

  override def isEmpty = seq.isEmpty
  override def size: Int = seq.size
  override def foreach(f: (K, V) => Unit): Unit = seq foreach f.tupled
}
