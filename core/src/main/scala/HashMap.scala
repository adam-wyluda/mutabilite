package offheap.collection

import scala.reflect.ClassTag

// Uses ideas from:
// https://doc.rust-lang.org/std/collections/struct.HashMap.html
// http://sebastiansylvan.com/post/robin-hood-hashing-should-be-your-default-hash-table-implementation/
// http://codecapsule.com/2013/11/11/robin-hood-hashing/
// http://codecapsule.com/2013/11/17/robin-hood-hashing-backward-shift-deletion/
class HashMap[K, V](initialSize: Int = 16)(
    implicit tagK: ClassTag[K], tagV: ClassTag[V])
    extends Map[K, V] {

  private[this] val hashes: Array[Int] = new Array[Int](initialSize)
  private[this] val _keys: Array[K] = new Array[K](initialSize)
  private[this] val _values: Array[V] = new Array[V](initialSize)
  private[this] var _size = 0
  private[this] var capacity = initialSize

  def apply(key: K): Opt[V] = {
    val index = indexOf(key)
    if (index == -1) {
      new Opt[V]()
    } else {
      new Opt[V](_values(index))
    }
  }

  private[this] def indexOf(key: K): Int = {
    var hash = hashCode(key)
    var pos = hash
    var result = -1
    while ({
      val nextHash = hashes(pos)
      if (!isInit(nextHash)) {
        val dis = (capacity + pos - hash) % capacity
        val nextDis = (capacity + pos - nextHash) % capacity
        if (nextDis >= dis) {
          if (_keys(pos) == key) {
            result = pos
            false
          } else {
            pos = (pos + 1) % capacity
            true
          }
        } else {
          false
        }
      } else {
        false
      }
    }) ()
    result
  }

  def put(key: K, value: V): Opt[V] = {
    var _key = key
    var _value = value
    var originalHash = hashCode(key)
    var hash = originalHash
    var pos = hash
    var previous = new Opt[V]()
    while ({
      val nextHash = hashes(pos)
      if (isInit(nextHash)) {
        init(pos, hash, key, value)
        _size += 1
        false
      } else if (nextHash == originalHash && _keys(pos) == key) {
        previous = new Opt[V](_values(pos))
        _values(pos) = value
        false
      } else {
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

  def remove(key: K): Opt[V] = {
    var index = indexOf(key)
    if (index != -1) {
      val previous = new Opt[V](_values(index))
      while ({
        val nextIndex = index + 1
        val nextHash = hashes(nextIndex)
        if (!isInit(nextHash)) {
          val nextDis = (capacity + nextIndex - nextHash)
          if (nextDis != 0) {
            hashes(index) = hashes(nextIndex)
            _keys(index) = _keys(nextIndex)
            _values(index) = _values(nextIndex)
            index = (index + 1) % capacity
            true
          } else {
            false
          }
        } else {
          false
        }
      }) ()
      _size -= 1
      previous
    } else {
      new Opt[V]()
    }
  }

  def keys: Set[K] = {
    val result = new NaiveSet[K]
    foreach { case (k, _) => result.add(k) }
    result
  }

  def values: Seq[V] = {
    val result = new NaiveSeq[V]
    foreach { case (_, v) => result.append(v) }
    result
  }

  def contains(key: K): Boolean = this(key).nonEmpty

  private[this] def hashCode(key: K) = {
    var hash = key.hashCode() % capacity
    hash |= (if (hash == 0) 1 else 0)
    hash
  }

  private[this] def isInit(hash: Int) = hash == 0

  private[this] def init(pos: Int, hash: Int, key: K, value: V) = {
    hashes(pos) = hash
    _keys(pos) = key
    _values(pos) = value
  }

  override def isEmpty = _size == 0

  override def size: Int = _size

  override def foreach[U](f: ((K, V)) => U): Unit = {
    var i = 0
    while (i < capacity) {
      val hash = hashes(i)
      if (!isInit(hash)) {
        val key = _keys(i)
        val value = _values(i)
        f((key, value))
      }
      i += 1
    }
  }
}
