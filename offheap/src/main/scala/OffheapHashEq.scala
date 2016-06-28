package offheap.collection

trait Eq_Offheap extends Eq[Long] {
  def eqv(a: Long, b: Long): Boolean = a == b
}

trait Hash_Offheap extends Eq_Offheap with Hash[Long] {
  def hash(value: Long): Int
}
