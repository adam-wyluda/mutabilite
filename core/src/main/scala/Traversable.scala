package offheap.collection

/**
  * Base trait for all collections.
  *
  * Provides basic information about collection size.
  *
  * To aid specialization, the foreach method definition is moved to [[offheap.collection.Traversable1]] and
  * [[offheap.collection.Traversable2]].
  */
sealed trait Traversable {

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

/**
  * Generic interface for collections that can be traversed over single elements.
  */
trait Traversable1[A] extends Traversable {

  /**
    * Calls given function for every value in collection.
    *
    * @param f function that is called with the collection values
    */
  def foreach(f: A => Unit): Unit
}

/**
  * Generic interface for collections that can be traversed over pairs of elements.
  */
trait Traversable2[A, B] extends Traversable {

  /**
    * Calls given function for every pair of values in collection.
    *
    * @param f function that is called with the value pairs from collection
    */
  def foreach(f: (A, B) => Unit): Unit
}
