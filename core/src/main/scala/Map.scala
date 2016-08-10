package offheap.collection
package generic

/**
  * Mutable map of keys to values.
  *
  * @tparam K type of the keys
  * @tparam V type of the values
  */
trait Map[K, V] extends Traversable2[K, V] {

  /**
    * Return value associated with given key.
    *
    * @param key given key
    * @return value associated with the key
    * @throws java.util.NoSuchElementException if element is not present in the map
    */
  def apply(key: K): V

  /**
    * Return option to value associated with given key.
    *
    * @param key given key
    * @return value associated with the key, None if the key is not found
    */
  def get(key: K): Option[V]

  /**
    * Insert value under given key to the map.
    *
    * @param key given key
    * @param value value to be associated with the key
    */
  def put(key: K, value: V): Unit

  /**
    * Remove key and value associated with the given key.
    *
    * @param key given key
    */
  def remove(key: K): Unit

  /**
    * Build the key set of this map.
    *
    * @return key set
    */
  def keys: Set[K]

  /**
    * Build value sequence of from this map.
    *
    * @return value sequence
    */
  def values: Seq[V]

  /**
    * Tests if the key set of this map contains given key.
    *
    * @param key key to be searched in this map
    * @return true if this map contains given key, false otherwise
    */
  def contains(key: K): Boolean

  /**
    * Reduce map size to the lowest capacity that allows storing all contained elements without growing.
    */
  def compact: Unit

  /**
    * Returns the size of underlying key array.
    *
    * @return size of underlying key array
    */
  def capacity: Int
}
