package offheap.collection

import scala.reflect.ClassTag

class NaiveSeq[A](initialSize: Int = 1)(implicit tag: ClassTag[A])
  extends Collection
    with Seq[A] {

  private[this] var array: Array[A] = new Array[A](initialSize)
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

  def append(that: Traversable[A]): Unit = {
    val newSize = _size + that.size
    growTo(newSize)
    that foreach { e =>
      array(_size) = e
      _size += 1
    }
  }

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
    Array.copy(array, 0, newArray, 0, _size)
    this.array = newArray
  }
  private def growTo(size: Int) = while (shouldGrow(size)) grow

  override def isEmpty = _size == 0
  override def size: Int = _size
  override def foreach[U](f: (A) => U): Unit =
    0 until _size foreach (a => f(array(a)))
}
