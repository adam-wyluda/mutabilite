package offheap.collection

/**
  * Mutable collection of distinct values.
  *
  * @tparam A represents element type
  */
trait Set[A] extends Traversable1[A] {

  /**
    * Test if given element is contained within this set.
    *
    * @param elem element to be tested
    * @return true if this set contains given element, false otherwise
    */
  def apply(elem: A): Boolean

  /**
    * Add element to the set.
    *
    * @param elem value to be added
    * @return true if this element was not present in the set, false otherwise
    */
  def add(elem: A): Boolean

  /**
    * Remove element from the set.
    *
    * @param elem element to be removed
    * @return true if element was removed from set, false otherwise
    */
  def remove(elem: A): Boolean

  /**
    * Reduce set size to the lowest capacity that allows storing all contained elements without growing.
    */
  def compact: Unit

  /**
    * Returns the size of underlying value array.
    *
    * @return size of underlying value array
    */
  def capacity: Int
}
