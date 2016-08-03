package offheap.collection

trait Eq[K] {
  def eqv(a: K, b: K): Boolean
}
