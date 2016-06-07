package offheap.collection

import scala.reflect.ClassTag

class BufferSeq[A](initialSize: Int = 16)(implicit tag: ClassTag[A])
    extends Seq[A] {

  private[this] var array: Array[AnyRef] = new Array[AnyRef](initialSize)
  private[this] var _size = 0

  def apply(index: Int): A = array(index).asInstanceOf[A]

  def append(elems: A*): Unit = {
    val newSize = _size + elems.size
    growTo(newSize)
    elems.foreach { e =>
      array(_size) = e.asInstanceOf[AnyRef]
      _size += 1
    }
  }

  def append(that: Traversable[A]): Unit = {
    val newSize = _size + that.size
    growTo(newSize)
    that foreach { e =>
      array(_size) = e.asInstanceOf[AnyRef]
      _size += 1
    }
  }

  def update(index: Int, value: A): Unit = array(index) = value.asInstanceOf[AnyRef]

  def remove(n: Int): A = {
    val removed = array(n)
    copy(n + 1, n, _size - n)
    _size -= 1
    removed.asInstanceOf[A]
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
    copy(index, index + 1, _size - index)
    array(index) = elem.asInstanceOf[AnyRef]
    _size += 1
  }

  private def copy(src: Int, dest: Int, len: Int) = System.arraycopy(array, src, array, dest, len)

  private def shouldGrow(newSize: Int) = newSize > array.size
  private def grow = {
    val newArray = new Array[AnyRef](array.size * 2)
    System.arraycopy(array, 0, newArray, 0, _size)
    this.array = newArray
  }
  private def growTo(size: Int) = while (shouldGrow(size)) grow

  override def isEmpty = _size == 0
  override def size: Int = _size
  override def foreach[U](f: (A) => U): Unit = {
    var i = 0
    while (i < _size) {
      f(array(i).asInstanceOf[A])
      i += 1
    }
  }
}
