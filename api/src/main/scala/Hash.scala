package offheap.collection

trait Hash[K] extends Eq[K] {
  def hash(value: K): Int
}
