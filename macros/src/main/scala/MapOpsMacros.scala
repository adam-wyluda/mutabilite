package offheap.collection.macros

import scala.reflect.macros.whitebox

class MapOpsMacros(val c: whitebox.Context) extends Common {

  import c.universe._
  import c.universe.definitions._

  def stabilizedMap[K: WeakTypeTag, V: WeakTypeTag](f: Tree => Tree): Tree =
    stabilized(c.prefix.tree) { pre =>
      val mapTpe = mapType[K, V]
      val map = freshVal("map", mapTpe, q"$pre.map")
      q"""
        $map
        ..${f(q"${map.symbol}")}
      """
    }

  def map[K: WeakTypeTag, V: WeakTypeTag, T: WeakTypeTag](f: Tree) =
    stabilizedMap[K, V] { map =>
      val builderTpe = bufferType[T]
      val builder = freshVal("builder",
                             builderTpe,
                             q"new $builderTpe(initialSize = $map.capacity)")
      val body = iterateHash(
          map,
          idx => q"""
              ${builder.symbol}.append(
                ${app(f, q"$map.keyAt($idx)", q"$map.valueAt($idx)")}
              )
            """
      )
      q"""
        $builder
        ..$body
        ${builder.symbol}
      """
    }

  def mapKeys[K: WeakTypeTag, V: WeakTypeTag, T: WeakTypeTag](f: Tree) =
    stabilizedMap[K, V] { map =>
      val builderTpe = hashMapType[T, V]
      val builder = freshVal("builder",
                             builderTpe,
                             q"new $builderTpe(initialSize = $map.capacity)")
      val body = iterateHash(
          map,
          idx => q"""
            ${builder.symbol}.put(
              ${app(f, q"$map.keyAt($idx)")},
              $map.valueAt($idx)
            )
          """
      )
      q"""
        $builder
        ..$body
        ${builder.symbol}
      """
    }

  def mapValues[K: WeakTypeTag, V: WeakTypeTag, T: WeakTypeTag](f: Tree) =
    stabilizedMap[K, V] { map =>
      val builderTpe = hashMapType[K, T]
      val builder = freshVal("builder",
                             builderTpe,
                             q"new $builderTpe(initialSize = $map.capacity)")
      val body = iterateHash(
          map,
          idx => q"""
            ${builder.symbol}.put(
              $map.keyAt($idx),
              ${app(f, q"$map.valueAt($idx)")}
            )
          """
      )
      q"""
        $builder
        ..$body
        ${builder.symbol}
      """
    }

  def flatMap[K: WeakTypeTag, V: WeakTypeTag, T: WeakTypeTag](f: Tree) =
    stabilizedMap[K, V] { map =>
      val resultTpe = seqType[T]
      val builderTpe = bufferType[T]
      val builder = freshVal("builder", resultTpe, q"new $builderTpe")
      val body = iterateHash(map, idx => {
        val result = freshVal("result",
                              resultTpe,
                              q"""
                                ${app(f,
                                      q"$map.keyAt($idx)",
                                      q"$map.valueAt($idx)")}
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
        $builder
        ..$body
        ${builder.symbol}
      """
    }

  def filter[K: WeakTypeTag, V: WeakTypeTag](f: Tree) =
    stabilizedMap[K, V] { map =>
      val K = weakTypeOf[K]
      val V = weakTypeOf[V]
      val builderTpe = hashMapType[K, V]
      val builder = freshVal("builder", builderTpe, q"new $builderTpe")
      val body = iterateHash(map, idx => {
        val key = freshVal("key", K, q"$map.keyAt($idx)")
        val value = freshVal("value", V, q"$map.valueAt($idx)")
        q"""
          $key
          $value
          if (${app(f, q"${key.symbol}", q"${value.symbol}")})
            ${builder.symbol}.put(${key.symbol}, ${value.symbol})
        """
      })
      q"""
        $builder
        ..$body
        ${builder.symbol}
      """
    }
}
