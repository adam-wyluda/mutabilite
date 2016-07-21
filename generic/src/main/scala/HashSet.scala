package offheap.collection

import scala.reflect.ClassTag

// Uses ideas from:
// https://doc.rust-lang.org/std/collections/struct.HashMap.html
// http://sebastiansylvan.com/post/robin-hood-hashing-should-be-your-default-hash-table-implementation/
// http://codecapsule.com/2013/11/11/robin-hood-hashing/
// http://codecapsule.com/2013/11/17/robin-hood-hashing-backward-shift-deletion/
class HashSet[A](initialSize: Int = 8)(implicit tag: ClassTag[A])
    extends GenericSet[A] {

  private[this] var hashes: Array[Int] = new Array[Int](initialSize)
  private[this] var _keys: Array[AnyRef] = new Array[AnyRef](initialSize)
  private[this] var _size = 0
  var capacity = initialSize
  private[this] var mask = capacity - 1

  def apply(key: A): Boolean = indexOf(key) != -1

  private[this] def indexOf(key: A): Int = {
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

  def add(key: A): Boolean = {
    var _key = key.asInstanceOf[AnyRef]
    var originalHash = hashCode(key)
    var hash = originalHash
    var pos = hash
    var dis = 0
    var result = true
    while ({
      val nextHash = hashes(pos)
      if (isInit(nextHash)) {
        hashes(pos) = hash
        _keys(pos) = _key
        _size += 1
        growIfNecessary
        result = true
        false
      } else if (nextHash == originalHash && _keys(pos) == key) {
        result = false
        false
      } else {
        val nextDis = (capacity + pos - nextHash) & mask
        if (nextDis < dis) {
          val nextKey = _keys(pos)
          hashes(pos) = hash
          _keys(pos) = _key
          hash = nextHash
          _key = nextKey
          dis = nextDis
        }
        dis += 1
        pos = (pos + 1) & mask
        true
      }
    }) ()
    result
  }

  def remove(key: A): Boolean = {
    var index = indexOf(key)
    if (index != -1) {
      while ({
        val nextIndex = (index + 1) & mask
        val nextHash = hashes(nextIndex)
        if (!isInit(nextHash)) {
          val nextDis = (capacity + nextIndex - nextHash) & mask
          if (nextDis != 0) {
            hashes(index) = hashes(nextIndex)
            _keys(index) = _keys(nextIndex)
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
      true
    } else {
      false
    }
  }

  def intersect(that: Set[A]): Set[A] = {
    val result = new HashSet[A]
    foreach { a =>
      if (that(a)) result.add(a)
    }
    result
  }

  def union(that: Set[A]): Set[A] = {
    val result = new HashSet[A]
    this foreach (result.add(_))
    that foreach (result.add(_))
    result
  }

  def diff(that: Set[A]): Set[A] = {
    val result = new HashSet[A]
    foreach { a =>
      if (!that(a)) result.add(a)
    }
    result
  }

  def map[B: ClassTag](f: A => B): HashSet[B] = {
    val builder = new HashSet[B](initialSize = capacity)
    var i = 0
    while (i < capacity) {
      val hash = hashes(i)
      if (!isInit(hash)) {
        val key = _keys(i).asInstanceOf[A]
        builder.add(f(key))
      }
      i += 1
    }
    builder
  }

  def flatMap[B: ClassTag](f: A => Traversable1[B]): HashSet[B] = {
    val builder = new HashSet[B]
    var i = 0
    while (i < capacity) {
      val hash = hashes(i)
      if (!isInit(hash)) {
        val key = _keys(i).asInstanceOf[A]
        val result = f(key)
        result foreach (builder.add(_))
      }
      i += 1
    }
    builder
  }

  def filter(f: A => Boolean): HashSet[A] = {
    val result = new HashSet[A]
    var i = 0
    while (i < capacity) {
      val hash = hashes(i)
      if (!isInit(hash)) {
        val key = _keys(i).asInstanceOf[A]
        if (f(key)) result.add(key)
      }
      i += 1
    }
    result
  }

  @inline
  private[this] def shouldGrow = _size > capacity * 9 / 10

  private[this] def growIfNecessary: Unit = {
    if (shouldGrow) {
      val oldCapacity = capacity
      val oldHashes = hashes
      val oldKeys = _keys
      capacity *= 2
      mask = capacity - 1
      hashes = new Array[Int](capacity)
      _keys = new Array[AnyRef](capacity)
      _size = 0
      var i = 0
      while (i < oldCapacity) {
        val hash = oldHashes(i)
        if (!isInit(hash)) {
          val key = oldKeys(i).asInstanceOf[A]
          add(key)
        }
        i += 1
      }
    }
  }

  private[this] def hashCode(key: A) = {
    var hash = key.hashCode() & mask
    hash |= (if (hash == 0) 1 else 0)
    hash
  }

  @inline
  private[this] def isInit(hash: Int) = hash == 0

  override def empty = _size == 0

  override def size: Int = _size

  override def foreach(f: A => Unit): Unit = {
    var i = 0
    while (i < capacity) {
      val hash = hashes(i)
      if (!isInit(hash)) {
        val key = _keys(i).asInstanceOf[A]
        f(key)
      }
      i += 1
    }
  }

  def foreachGeneric[U](f: A => U): Unit = {
    var i = 0
    while (i < capacity) {
      val hash = hashes(i)
      if (!isInit(hash)) {
        val key = _keys(i).asInstanceOf[A]
        f(key)
      }
      i += 1
    }
  }
}
