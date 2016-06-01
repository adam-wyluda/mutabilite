package offheap.collection

class Opt[A] extends Traversable[A] {

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
