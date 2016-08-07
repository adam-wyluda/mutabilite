package offheap.collection

/**
  * Typeclass implementing element equality.
  *
  * Import [[offheap.collection]] package object to get default implicit implementations for primitive and reference
  * types.
  *
  * @tparam K type of compared instances
  */
trait Eq[K] {
  def eqv(a: K, b: K): Boolean
}

/**
  * Typeclass that provides hash code values for specified type.
  *
  * Import [[offheap.collection]] package object to get default implicit implementations for primitive and reference
  * types.
  *
  * @tparam K type of hashed instances
  */
trait Hash[K] extends Eq[K] {
  def hash(value: K): Int
}
