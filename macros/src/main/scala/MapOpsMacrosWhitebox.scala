package mutabilite.macros

import scala.reflect.macros.whitebox

class MapOpsMacrosWhitebox(override val c: whitebox.Context) extends MapOpsMacros(c) {

  import c.universe._
  import c.universe.definitions._

  def map[B: WeakTypeTag](f: Tree) =
    stabilizedMap { map =>
      val arrTpe =
        if (weakTypeOf[B] <:< AnyRefTpe) weakTypeOf[Array[AnyRef]]
        else weakTypeOf[Array[B]]
      val resultTpe = seqType[B]
      val arr = freshVal("array", arrTpe, q"new $arrTpe($map.size)")
      val idx2 = freshVar("j", IntTpe, q"0")
      val body = iterateHash(
        map,
        idx => q"""
              ${arr.symbol}(${idx2.symbol}) = ${app(f, q"$map.keyAt($idx)", q"$map.valueAt($idx)")}
              ${idx2.symbol} += 1
            """
      )
      q"""
        $arr
        $idx2
        ..$body
        new $resultTpe(${arr.symbol}, $map.size)
      """
    }

  def mapKeys[V: WeakTypeTag, B: WeakTypeTag](f: Tree) =
    stabilizedMap { map =>
      val builderTpe = mapType[B, V]
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

  def mapValues[K: WeakTypeTag, B: WeakTypeTag](f: Tree) =
    stabilizedMap { map =>
      val builderTpe = mapType[K, B]
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

  def flatMap[B: WeakTypeTag](f: Tree) =
    stabilizedMap { map =>
      val resultTpe = seqType[B]
      val builderTpe = seqType[B]
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
    stabilizedMap { map =>
      val K = weakTypeOf[K]
      val V = weakTypeOf[V]
      val builderTpe = mapType[K, V]
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
