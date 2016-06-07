package offheap.collection

import scala.reflect.ClassTag

class BufferSeq[A](initialSize: Int = 16)(implicit tag: ClassTag[A])
    extends Seq[A] {

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

  def update(index: Int, value: A): Unit = array(index) = value

  def remove(n: Int): A = {
    val removed = array(n)
    var i = n
    while (i < _size) {
      array(i) = array(i + 1)
      i += 1
    }
    _size -= 1
    removed
  }

  def index(elem: A): Int = {
    var result = -1
    var i = 0
    while ({
      if (i < _size) {
        if (array(i) == elem) {
          result = i
          false
        } else {
          i += 1
          true
        }
      } else {
        false
      }
    }) ()
    result
  }

  def insert(index: Int, elem: A): Unit = {
    val newSize = _size + 1
    growTo(newSize)
    var i = _size - 1
    while (i >= index) {
      array(i + 1) = array(i)
      i -= 1
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
  override def foreach[U](f: (A) => U): Unit = {
    var i = 0
    while (i < _size) {
      f(array(i))
      i += 1
    }
  }
}
