package offheap.collection.macros

import scala.reflect.macros.whitebox

class SeqOpsMacros(val c: whitebox.Context) extends Common {
  import c.universe._
  import c.universe.definitions._

  def builderType[T: WeakTypeTag]: TypeName = TypeName("BufferSeq_" + typeName[T])

  def map[B: WeakTypeTag](f: Tree) =
    stabilized(c.prefix.tree) { pre =>
      val builder = builderType[B]
      q"""
        val builder = new $builder(initialSize = $pre.seq.size)
        var i = 0
        val size = $pre.seq.size
        while (i < size) {
          builder.append(${app(f, q"$pre.seq(i)")})
          i += 1
        }
        builder
      """
    }

  def flatMap[B: WeakTypeTag](f: Tree) =
    stabilized(c.prefix.tree) { pre =>
      val builder = builderType[B]
      q"""
        val builder = new $builder
        var i = 0
        val size = $pre.seq.size
        while (i < size) {
          val el = $pre.seq(i)
          val result: $builder = ${app(f, q"el")}
          val resultSize = result.size
          var j = 0
          while (j < resultSize) {
            builder.append(result(j))
            j += 1
          }
          i += 1
        }
        builder
      """
    }

  def filter[A: WeakTypeTag](f: Tree) =
    stabilized(c.prefix.tree) { pre =>
      val builder = builderType[A]
      q"""
        val result = new $builder
        var i = 0
        val size = $pre.seq.size
        while (i < size) {
          val el = $pre.seq(i)
          if (${app(f, q"el")}) result.append(el)
          i += 1
        }
        result
      """
    }
}
