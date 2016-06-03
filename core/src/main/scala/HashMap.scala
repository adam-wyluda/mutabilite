package offheap.collection

// Uses ideas from:
// https://doc.rust-lang.org/std/collections/struct.HashMap.html
// http://sebastiansylvan.com/post/robin-hood-hashing-should-be-your-default-hash-table-implementation/
// http://codecapsule.com/2013/11/11/robin-hood-hashing/
// http://codecapsule.com/2013/11/17/robin-hood-hashing-backward-shift-deletion/
class HashMap[K, V](initialSize: Int = 16) extends Map[K, V] {

  private[this] val hashes: Array[Int] = new Array[Int](initialSize)
  private[this] val _keys: Array[K] = new Array[K](initialSize)
  private[this] val _values: Array[V] = new Array[V](initialSize)
  private[this] var _size = 0
  private[this] var capacity = initialSize

  def apply(key: K): Opt[V] = ???

  def put(key: K, value: V): Opt[V] = {
    var _key = key
    var _value = value
    var hash = hashCode(key)
    var pos = hash % capacity
    var previous = new Opt[V]()
    while ({
      if (isInit(pos)) {
        init(pos, hash, key, value)
        false
      } else {
        val nextHash = hashes(pos)
        val dis = (capacity + pos - hash) % capacity
        val nextDis = (capacity + pos - nextHash) % capacity
        if (nextDis < dis) {
          val nextKey = _keys(pos)
          val nextVal = _values(pos)
          hashes(pos) = hash
          _keys(pos) = _key
          _values(pos) = _value
          hash = nextHash
          _key = nextKey
          _value = nextVal
        }
        pos = (pos + 1) % capacity
        true
      }
    }) ()
    previous
  }

  def remove(key: K): Opt[V] = ???

  def keys: NaiveSet[K] = ???

  def values: NaiveSeq[V] = ???

  def contains(key: K): Boolean = this(key).nonEmpty

  private[this] def hashCode(key: K) = {
    var hash = key.hashCode()
    hash &= 0x7fffffff
    hash |= (if (hash == 0) 1 else 0)
    hash
  }

  private[this] def isInit(pos: Int) = hashes(pos) == 0

  private[this] def isDeleted(hash: Int) = (hash >> 31) != 0

  private[this] def init(pos: Int, hash: Int, key: K, value: V) = {
    hashes(pos) = hash
    _keys(pos) = key
    _values(pos) = value
  }

  override def isEmpty = _size == 0

  override def size: Int = _size

  override def foreach[U](f: ((K, V)) => U): Unit = ???
}
