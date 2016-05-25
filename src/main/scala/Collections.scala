package offheap.collection

import scala.collection.generic.CanBuildFrom
import scala.{collection => onheap}

trait Collection extends Any {
//  def addr: Long

  def readSize: Int = ???
  def isEmpty: Boolean = ???
  def nonEmpty: Boolean = !isEmpty
}

trait Traversable[A] extends Any {
  def size: Int
  def isEmpty: Boolean
  def nonEmpty: Boolean
  def foreach[U](f: A => U)

  def to[T[_]](implicit canBuild: CanBuildFrom[_, A, T[A]]): T[A] =
    to[T, A](identity)
  def to[T[_], B](map: A => B)(
      implicit canBuild: CanBuildFrom[_, B, T[B]]): T[B] = {
    val builder = canBuild()
    foreach(builder += map(_))
    builder.result()
  }
}

object Test extends App {
  val s = new Seq[Int]()
  val o = s.to[onheap.Set]
  println(o.contains(3))
}

// For every off-heap collection
object Companion {

  def from[A](seq: onheap.Seq[A]): Traversable[A] = from[A, A](seq, identity)
  def from[A, B](seq: onheap.Seq[A], map: A => B): Traversable[B] = ???
}

class Opt[A] extends Collection with Traversable[A] {

  private[this] class Holder(val elem: A)

  private[this] var holder: Holder = null

  def this(elem: A) = {
    this()
    holder = new Holder(elem)
  }

  // Dereference primitive or cast self to data class
  def get(): A =
    if (nonEmpty) holder.elem else throw new NoSuchElementException

  override def isEmpty: Boolean = holder == null
  override def size: Int = if (nonEmpty) 1 else 0
  override def foreach[U](f: (A) => U): Unit = if (nonEmpty) f(get())
}

class Seq[A] extends Collection with Traversable[A] {

  def apply(index: Int): A = ???

  def append(elems: A*): Unit = ???
  def append(offheap: Traversable[A]): Unit = ???
  def update(index: Int, value: A): Unit = ???
  def remove(n: Int): A = ???

  def index(elem: A): Int = ???
  def insert(index: Int, elem: A) = ???

  override def size: Int = readSize
  override def foreach[U](f: (A) => U): Unit = ???
}

class Set[A] extends Collection with Traversable[A] {

  def apply(elem: A): Boolean = ???

  def add(elem: A): Boolean = ???
  def remove(elem: A): Boolean = ???

  def intersect(that: Set[A]): Set[A] = ???
  def union(that: Set[A]): Set[A] = ???
  def diff(that: Set[A]): Set[A] = ???

  override def size: Int = ???
  override def foreach[U](f: (A) => U): Unit = ???
}

class Map[K, V] extends Collection with Traversable[(K, V)] {

  def apply(key: K): Opt[V] = ???

  def put(key: K, value: V): Opt[V] = ???
  def remove(key: K): Opt[V] = ???

  def keys: Set[K] = ???
  def values: Seq[V] = ???

  // May be redundant since apply returns an option
  def contains(key: K): Boolean = this(key).nonEmpty

  override def size: Int = ???
  override def foreach[U](f: ((K, V)) => U): Unit = ???
}
