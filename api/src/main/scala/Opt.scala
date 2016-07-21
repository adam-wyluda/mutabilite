package offheap.collection

trait Opt[A] extends Any with Traversable1[A] {
  def get: A
  def size: Int
  def capacity = 1
}

class Some[A](value: A) extends Opt[A] {
  def get = value
  def empty = false

  def size = 1
  def foreach(f: A => Unit) = f(value)
}

class None[A] extends Opt[A] {
  def get = throw new NoSuchElementException
  def empty = true

  def size = 0
  def foreach(f: A => Unit) = ()
}
