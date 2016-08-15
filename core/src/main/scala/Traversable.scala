package mutabilite
package generic

/**
  * Base trait for all collections.
  *
  * Provides basic information about collection size.
  */
trait Traversable {

  /**
    * Size of the collection.
    *
    * @return number of elements in this collection
    */
  def size: Int

  /**
    * Tests if collection is empty.
    *
    * @return true if this collection size is equal to 0, false otherwise
    */
  def isEmpty: Boolean

  /**
    * Tests if collection is not empty.
    *
    * @return true if this collection contains one or more elements, false otherwise
    */
  def nonEmpty: Boolean = !isEmpty
}
