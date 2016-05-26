package offheap.collection

import scala.collection.generic.CanBuildFrom
import scala.reflect.ClassTag
import scala.{collection => onheap}

trait Collection extends Any {
//  def addr: Long

  def readSize: Int = ???
  def isEmpty: Boolean = ???
  def nonEmpty: Boolean = !isEmpty
}

trait Traversable[A] extends Any {
  def size: Int
  def isEmpty: Boolean
  def nonEmpty: Boolean
  def foreach[U](f: A => U)

  def to[T[_]](implicit canBuild: CanBuildFrom[_, A, T[A]]): T[A] =
    to[T, A](identity)
  def to[T[_], B](map: A => B)(
      implicit canBuild: CanBuildFrom[_, B, T[B]]): T[B] = {
    val builder = canBuild()
    foreach(builder += map(_))
    builder.result()
  }
}

object Test extends App {
  val s = new Seq[Int]()
  val o = s.to[onheap.Set]
  println(o.contains(3))
}

// For every off-heap collection
object Companion {

  def from[A](seq: onheap.Seq[A]): Traversable[A] = from[A, A](seq, identity)
  def from[A, B](seq: onheap.Seq[A], map: A => B): Traversable[B] = ???
}

class Opt[A] extends Collection with Traversable[A] {

  private[this] class Holder(val elem: A)

  private[this] var holder: Holder = null

  def this(elem: A) = {
    this()
    holder = new Holder(elem)
  }

  // Dereference primitive or cast self to data class
  def get(): A =
    if (nonEmpty) holder.elem else throw new NoSuchElementException

  override def isEmpty: Boolean = holder == null
  override def size: Int = if (nonEmpty) 1 else 0
  override def foreach[U](f: (A) => U): Unit = if (nonEmpty) f(get())
}

class Seq[A](implicit tag: ClassTag[A])
    extends Collection with Traversable[A] {

  private[this] var array: Array[A] = new Array[A](1)
  private[this] var _size = 0

  def apply(index: Int): A = array(index)

  def append(elems: A*): Unit = {
    val newSize = _size + elems.size
    growTo(newSize)
    elems.foreach { e =>
      array(_size) = e
      _size += 1
    }
  }

  def append(offheap: Traversable[A]): Unit = ???

  def update(index: Int, value: A): Unit = {
    array(index) = value
  }

  def remove(n: Int): A = {
    val removed = array(n)
    n until (_size - 1) foreach { i =>
      array(i) = array(i + 1)
    }
    _size -= 1
    removed
  }

  def index(elem: A): Int = {
    var result = -1
    0 until _size foreach { i =>
      if (result == -1 && array(i) == elem) result = i
    }
    result
  }

  def insert(index: Int, elem: A): Unit = {
    val newSize = _size + 1
    growTo(newSize)
    (_size - 1) to index by -1 foreach { i =>
      array(i + 1) = array(i)
    }
    array(index) = elem
    _size += 1
  }

  private def shouldGrow(newSize: Int) = newSize > array.size
  private def grow = {
    val newArray = new Array[A](array.size * 2)
    Array.copy(array, 0, newArray, 0, array.size)
    this.array = newArray
  }
  private def growTo(size: Int) = while (shouldGrow(size)) grow

  override def isEmpty = _size == 0
  override def size: Int = _size
  override def foreach[U](f: (A) => U): Unit =
    0 until _size foreach (a => f(array(a)))
}

class Set[A](implicit tag: ClassTag[A])
    extends Collection with Traversable[A] {

  private[this] val seq = new Seq[A]

  def apply(elem: A): Boolean = seq.index(elem) != -1

  def add(elem: A): Boolean = {
    if (this(elem)) {
      false
    } else {
      seq.append(elem)
      true
    }
  }

  def remove(elem: A): Boolean = {
    if (this(elem)) {
      seq.remove(seq.index(elem))
      true
    } else {
      false
    }
  }

  def intersect(that: Set[A]): Set[A] = {
    val result = new Set[A]
    foreach { a =>
      if (that(a)) result.add(a)
    }
    result
  }

  def union(that: Set[A]): Set[A] = {
    val result = new Set[A]
    this foreach (result.add(_))
    that foreach (result.add(_))
    result
  }

  def diff(that: Set[A]): Set[A] = {
    val result = new Set[A]
    foreach { a =>
      if (!that(a)) result.add(a)
    }
    result
  }

  override def isEmpty = seq.isEmpty
  override def size: Int = seq.size
  override def foreach[U](f: (A) => U): Unit = seq foreach f
}

class Map[K, V](implicit tag: ClassTag[(K, V)])
    extends Collection with Traversable[(K, V)] {

  private[this] val seq = new Seq[(K, V)]

  def apply(key: K): Opt[V] = {
    var result = new Opt[V]
    seq foreach {
      case (k, v) =>
        if (k == key) result = new Opt[V](v)
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

  def keys(implicit tag: ClassTag[K]): Set[K] = {
    val result = new Set[K]
    foreach { case (k, v) => result.add(k) }
    result
  }

  def values(implicit tag: ClassTag[V]): Seq[V] = {
    val result = new Seq[V]
    foreach { case (k, v) => result.append(v) }
    result
  }

  // May be redundant since apply returns an option
  def contains(key: K): Boolean = this(key).nonEmpty

  private def keyIndex(key: K) = {
    var result = -1
    var index = 0
    seq foreach { case (k, v) => if (k == key) result = index else index += 1 }
    result
  }

  override def isEmpty = seq.isEmpty
  override def size: Int = seq.size
  override def foreach[U](f: ((K, V)) => U): Unit = seq foreach f
}
