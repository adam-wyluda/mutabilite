package offheap.collection

trait Eq[K] {
  def eqv(a: K, b: K): Boolean
}

trait Hash[K] extends Eq[K] {
  def hash(value: K): Int
}
