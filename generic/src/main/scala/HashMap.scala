package offheap.collection

import scala.reflect.ClassTag

// Uses ideas from:
// https://doc.rust-lang.org/std/collections/struct.HashMap.html
// http://sebastiansylvan.com/post/robin-hood-hashing-should-be-your-default-hash-table-implementation/
// http://codecapsule.com/2013/11/11/robin-hood-hashing/
// http://codecapsule.com/2013/11/17/robin-hood-hashing-backward-shift-deletion/
class HashMap[K, V](initialSize: Int = 8)(
    implicit tagK: ClassTag[K], tagV: ClassTag[V])
    extends Map[K, V] {

  private[this] var hashes: Array[Int] = new Array[Int](initialSize)
  private[this] var _keys: Array[AnyRef] = new Array[AnyRef](initialSize)
  private[this] var _values: Array[AnyRef] = new Array[AnyRef](initialSize)
  private[this] var _size = 0
  private[this] var capacity = initialSize
  private[this] var mask = capacity - 1

  private[this] val emptyOpt = new None[V]

  def apply(key: K): Opt[V] = {
    val index = indexOf(key)
    if (index == -1) {
      emptyOpt
    } else {
      new Some[V](_values(index).asInstanceOf[V])
    }
  }

  private[this] def indexOf(key: K): Int = {
    var hash = hashCode(key)
    var pos = hash
    var dis = 0
    var result = -1
    while ({
      val nextHash = hashes(pos)
      if (!isInit(nextHash)) {
        val nextDis = (capacity + pos - nextHash) & mask
        if (nextDis >= dis) {
          if (hash == nextHash && _keys(pos) == key) {
            result = pos
            false
          } else {
            dis += 1
            pos = (pos + 1) & mask
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
    var _key = key.asInstanceOf[AnyRef]
    var _value = value.asInstanceOf[AnyRef]
    var originalHash = hashCode(key)
    var hash = originalHash
    var pos = hash
    var dis = 0
    var previous: Opt[V] = new None[V]
    while ({
      val nextHash = hashes(pos)
      if (isInit(nextHash)) {
        init(pos, hash, _key, _value)
        _size += 1
        growIfNecessary
        false
      } else if (nextHash == originalHash && _keys(pos) == key) {
        previous = new Some[V](_values(pos).asInstanceOf[V])
        _values(pos) = value.asInstanceOf[AnyRef]
        false
      } else {
        val nextDis = (capacity + pos - nextHash) & mask
        if (nextDis < dis) {
          val nextKey = _keys(pos)
          val nextVal = _values(pos)
          hashes(pos) = hash
          _keys(pos) = _key
          _values(pos) = _value
          hash = nextHash
          _key = nextKey
          _value = nextVal
          dis = nextDis
        }
        dis += 1
        pos = (pos + 1) & mask
        true
      }
    }) ()
    previous
  }

  def remove(key: K): Opt[V] = {
    var index = indexOf(key)
    if (index != -1) {
      val previous = new Some[V](_values(index).asInstanceOf[V])
      while ({
        val nextIndex = (index + 1) & mask
        val nextHash = hashes(nextIndex)
        if (!isInit(nextHash)) {
          val nextDis = (capacity + nextIndex - nextHash) & mask
          if (nextDis != 0) {
            hashes(index) = hashes(nextIndex)
            _keys(index) = _keys(nextIndex)
            _values(index) = _values(nextIndex)
            index = nextIndex
            true
          } else {
            false
          }
        } else {
          false
        }
      }) ()
      hashes(index) = 0
      _size -= 1
      previous
    } else {
      emptyOpt
    }
  }

  def keys: Set[K] = {
    val result = new MapSet[K]
    var i = 0
    while (i < capacity) {
      if (!isInit(hashes(i))) result.add(_keys(i).asInstanceOf[K])
      i += 1
    }
    result
  }

  def values: Seq[V] = {
    val result = new BufferSeq[V]
    var i = 0
    while (i < capacity) {
      if (!isInit(hashes(i))) result.append(_values(i).asInstanceOf[V])
      i += 1
    }
    result
  }

  def contains(key: K): Boolean = this(key).nonEmpty

  @inline
  private[this] def shouldGrow = _size > capacity * 9 / 10

  private[this] def growIfNecessary: Unit = {
    if (shouldGrow) {
      val oldCapacity = capacity
      val oldHashes = hashes
      val oldKeys = _keys
      val oldValues = _values
      capacity *= 2
      mask = capacity - 1
      hashes = new Array[Int](capacity)
      _keys = new Array[AnyRef](capacity)
      _values = new Array[AnyRef](capacity)
      _size = 0
      var i = 0
      while (i < oldCapacity) {
        val hash = oldHashes(i)
        if (!isInit(hash)) {
          val key = oldKeys(i)
          val value = oldValues(i)
          put(key.asInstanceOf[K], value.asInstanceOf[V])
        }
        i += 1
      }
    }
  }

  private[this] def hashCode(key: K) = {
    var hash = key.hashCode() & mask
    hash |= (if (hash == 0) 1 else 0)
    hash
  }

  @inline
  private[this] def isInit(hash: Int) = hash == 0

  private[this] def init(pos: Int, hash: Int, key: AnyRef, value: AnyRef) = {
    hashes(pos) = hash
    _keys(pos) = key
    _values(pos) = value
  }

  override def isEmpty = _size == 0

  override def size: Int = _size

  override def foreach(f: (K, V) => Unit): Unit = {
    var i = 0
    while (i < capacity) {
      val hash = hashes(i)
      if (!isInit(hash)) {
        val key = _keys(i).asInstanceOf[K]
        val value = _values(i).asInstanceOf[V]
        f(key, value)
      }
      i += 1
    }
  }

  def foreachGeneric[U](f: ((K, V)) => U): Unit = {
    var i = 0
    while (i < capacity) {
      val hash = hashes(i)
      if (!isInit(hash)) {
        val key = _keys(i).asInstanceOf[K]
        val value = _values(i).asInstanceOf[V]
        f((key, value))
      }
      i += 1
    }
  }
}
