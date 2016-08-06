package offheap.collection

trait Seq[A] extends Any with Traversable1[A] {
  def apply(index: Int): A
  def append(elem: A): Unit
  def update(index: Int, value: A): Unit
  def remove(n: Int): A
  def index(elem: A): Int
  def insert(index: Int, elem: A): Unit
  def compact: Unit
  def capacity: Int
}
