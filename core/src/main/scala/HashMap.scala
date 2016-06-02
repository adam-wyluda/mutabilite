package offheap.collection

// Uses ideas from:
// https://doc.rust-lang.org/std/collections/struct.HashMap.html
// http://sebastiansylvan.com/post/robin-hood-hashing-should-be-your-default-hash-table-implementation/
// http://codecapsule.com/2013/11/11/robin-hood-hashing/
class HashMap[K, V](initialSize: Int = 16) extends Map[K, V] {

  private[this] val hashes: Array[Int] = new Array[Int](initialSize)
  private[this] val _keys: Array[K] = new Array[K](initialSize)
  private[this] val _values: Array[V] = new Array[V](initialSize)
  private[this] var _size = 0

  def apply(key: K): Opt[V] = ???

  def put(key: K, value: V): Opt[V] = ???

  def remove(key: K): Opt[V] = ???

  def keys: NaiveSet[K] = ???

  def values: NaiveSeq[V] = ???

  def contains(key: K): Boolean = this(key).nonEmpty

  private[this] def capacity = _keys.size
  private[this] def mask = _keys.size - 1

  private[this] def init(pos: Int, hash: Int, key: K, value: V) = {
    hashes(pos) = hash
    _keys(pos) = key
    _values(pos) = value
  }



  override def isEmpty = _size == 0

  override def size: Int = _size

  override def foreach[U](f: ((K, V)) => U): Unit = ???
}
