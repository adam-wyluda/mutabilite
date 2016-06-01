package offheap.collection

class Opt[A] extends Traversable[A] {

  private[this] var value: A = _
  private[this] var empty: Boolean = true

  def this(elem: A) = {
    this()
    this.value = elem
    this.empty = false
  }

  // Dereference primitive or cast self to data class
  def get(): A =
    if (nonEmpty) value else throw new NoSuchElementException

  override def isEmpty: Boolean = empty
  override def size: Int = if (nonEmpty) 1 else 0
  override def foreach[U](f: (A) => U): Unit = if (nonEmpty) f(get())
}
