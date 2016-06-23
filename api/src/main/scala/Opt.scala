package offheap.collection

trait Opt[A] extends Traversable1[A] {
  def get: A
  def size = if (nonEmpty) 1 else 0
}

class Some[A](value: A) extends Opt[A] {
  def get = value
  def isEmpty = false
  def foreach(f: A => Unit) = f(get)
}

class None[A] extends Opt[A] {
  def get = throw new NoSuchElementException
  def isEmpty = true
  def foreach(f: A => Unit) = ()
}
