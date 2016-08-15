package mutabilite
package generic

/**
  * Mutable indexed sequence of values.
  *
  * @tparam A represents element type
  */
trait Seq[A] extends Traversable {

  /**
    * Return value at given index.
    *
    * @param index element index
    * @return value at given index
    */
  def apply(index: Int): A

  /**
    * Add value to the collection.
    *
    * @param elem value to be added to collection
    */
  def append(elem: A): Unit

  /**
    * Update value at given index.
    *
    * @param index updated element index
    * @param value value of the updated element
    */
  def update(index: Int, value: A): Unit

  /**
    * Remove element at given index
    *
    * @param index index of the element to be removed
    */
  def remove(index: Int): A

  /**
    * Find given element and returns its index. If not present in the collection, return -1.
    *
    * @param elem element that will be searched
    * @return index at which given element occurs, -1 if not present
    */
  def index(elem: A): Int

  /**
    * Insert element at given position.
    *
    * @param index target index
    * @param elem element to be inserted
    */
  def insert(index: Int, elem: A): Unit

  /**
    * Reduce sequence size to the lowest capacity that allows storing all contained elements.
    */
  def compact: Unit

  /**
    * Returns the number of elements this sequence can store before growing.
    *
    * @return capacity of this sequence
    */
  def capacity: Int
}
