package offheap.collection.macros

import scala.reflect.macros.whitebox

class MapOpsMacros(val c: whitebox.Context) extends Common {

  import c.universe._
  import c.universe.definitions._

  def map[K: WeakTypeTag, V: WeakTypeTag, T: WeakTypeTag](f: Tree) =
    stabilized(c.prefix.tree) { pre =>
      val mapTpe = mapType[K, V]
      val builderTpe = bufferType[T]
      val map = freshVal("map", mapTpe, q"$pre.map")
      val builder =
        freshVal("builder",
                 builderTpe,
                 q"new $builderTpe(initialSize = ${map.symbol}.capacity)")
      val body = iterateHash(
          q"${map.symbol}",
          idx =>
            q"""
              ${builder.symbol}.append(
                ${app(f,
                      q"${map.symbol}.keyAt($idx)",
                      q"${map.symbol}.valueAt($idx)")}
              )
            """
      )
      q"""
        $map
        $builder
        ..$body
        ${builder.symbol}
      """
    }

  def mapKeys[K: WeakTypeTag, V: WeakTypeTag, T: WeakTypeTag](f: Tree) =
    stabilized(c.prefix.tree) { pre =>
      val mapTpe = mapType[K, V]
      val builderTpe = hashMapType[T, V]
      val map = freshVal("map", mapTpe, q"$pre.map")
      val builder =
        freshVal("builder",
                 builderTpe,
                 q"new $builderTpe(initialSize = ${map.symbol}.capacity)")
      val body = iterateHash(
          q"${map.symbol}",
          idx => q"""
            ${builder.symbol}.put(
              ${app(f, q"${map.symbol}.keyAt($idx)")},
              ${map.symbol}.valueAt($idx)
            )
          """
      )
      q"""
        $map
        $builder
        ..$body
        ${builder.symbol}
      """
    }

  def mapValues[K: WeakTypeTag, V: WeakTypeTag, T: WeakTypeTag](f: Tree) =
    stabilized(c.prefix.tree) { pre =>
      val mapTpe = mapType[K, V]
      val builderTpe = hashMapType[K, T]
      val map = freshVal("map", mapTpe, q"$pre.map")
      val builder =
        freshVal("builder",
                 builderTpe,
                 q"new $builderTpe(initialSize = ${map.symbol}.capacity)")
      val body = iterateHash(
          q"${map.symbol}",
          idx => q"""
            ${builder.symbol}.put(
              ${map.symbol}.keyAt($idx),
              ${app(f, q"${map.symbol}.valueAt($idx)")}
            )
          """
      )
      q"""
        $map
        $builder
        ..$body
        ${builder.symbol}
      """
    }

  def flatMap[K: WeakTypeTag, V: WeakTypeTag, T: WeakTypeTag](f: Tree) =
    stabilized(c.prefix.tree) { pre =>
      val mapTpe = mapType[K, V]
      val resultTpe = seqType[T]
      val builderTpe = bufferType[T]
      val map = freshVal("map", mapTpe, q"$pre.map")
      val builder = freshVal("builder", resultTpe, q"new $builderTpe")
      val body = iterateHash(q"${map.symbol}", idx => {
        val result = freshVal("result",
                              resultTpe,
                              q"""
                                ${app(f,
                                      q"${map.symbol}.keyAt($idx)",
                                      q"${map.symbol}.valueAt($idx)")}
                                """)
        val inner = iterateSeq(
            q"${result.symbol}",
            idx2 => q"${builder.symbol}.append(${result.symbol}($idx2))"
        )
        q"""
          $result
          ..$inner
        """
      })
      q"""
        $map
        $builder
        ..$body
        ${builder.symbol}
      """
    }

  def filter[K: WeakTypeTag, V: WeakTypeTag](f: Tree) =
    stabilized(c.prefix.tree) { pre =>
      val K = weakTypeOf[K]
      val V = weakTypeOf[V]
      val mapTpe = mapType[K, V]
      val builderTpe = hashMapType[K, V]
      val map = freshVal("map", mapTpe, q"$pre.map")
      val builder = freshVal("builder", builderTpe, q"new $builderTpe")
      val body = iterateHash(q"${map.symbol}", idx => {
        val key = freshVal("key", K, q"${map.symbol}.keyAt($idx)")
        val value = freshVal("value", V, q"${map.symbol}.valueAt($idx)")
        q"""
          $key
          $value
          if (${app(f, q"${key.symbol}", q"${value.symbol}")})
            ${builder.symbol}.put(${key.symbol}, ${value.symbol})
        """
      })
      q"""
        $map
        $builder
        ..$body
        ${builder.symbol}
      """
    }
}
