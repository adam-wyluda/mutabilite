package offheap.collection.macros

import scala.reflect.macros.whitebox

class MapOpsMacros(val c: whitebox.Context) extends Common {

  import c.universe._
  import c.universe.definitions._

  def stabilizedMap(f: Tree => Tree): Tree =
    c.prefix.tree match {
      case Apply(_, List(map)) =>
        stabilized(map) { map =>
          q"""
            ..${f(q"${map.symbol}")}
          """
        }
    }

  def map[T: WeakTypeTag](f: Tree) =
    stabilizedMap { map =>
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

  def mapKeys[V: WeakTypeTag, T: WeakTypeTag](f: Tree) =
    stabilizedMap { map =>
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

  def mapValues[K: WeakTypeTag, T: WeakTypeTag](f: Tree) =
    stabilizedMap { map =>
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

  def flatMap[T: WeakTypeTag](f: Tree) =
    stabilizedMap { map =>
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
    stabilizedMap { map =>
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

  def foreach(f: Tree) =
    stabilizedMap { map =>
      iterateHash(map,
                  idx => app(f, q"$map.keyAt($idx)", q"$map.valueAt($idx)"))
    }

  def fold[B: WeakTypeTag](z: Tree)(op: Tree) =
    stabilizedMap { map =>
      val accTpe = weakTypeOf[B]
      val acc = freshVar("acc", accTpe, q"$z")
      val body = iterateHash(
          map,
          idx =>
            q"${acc.symbol} = ${app(op, q"${acc.symbol}", q"$map.keyAt($idx)", q"$map.valueAt($idx)")}")
      q"""
        $acc
        ..$body
        ${acc.symbol}
      """
    }

  def reduceKeys[K: WeakTypeTag](op: Tree) =
    stabilizedMap { map =>
      reduceHash[K](map, op, TermName("keyAt"))
    }

  def reduceValues[V: WeakTypeTag](op: Tree) =
    stabilizedMap { map =>
      reduceHash[V](map, op, TermName("valueAt"))
    }

  def transformValues(f: Tree) =
    stabilizedMap { map =>
      iterateHash(
          map,
          idx => q"$map.updateValue($idx, ${app(f, q"$map.valueAt($idx)")})")
    }

  def forall(p: Tree) =
    stabilizedMap { map =>
      val result = freshVar("result", BooleanTpe, q"true")
      val body = iterateHashWhile(
          map,
          q"${result.symbol}",
          idx =>
            q"${result.symbol} = ${app(p, q"$map.keyAt(${idx.symbol})", q"$map.valueAt(${idx.symbol})")}")
      q"""
        $result
        ..$body
        ${result.symbol}
      """
    }

  def exists(p: Tree) =
    stabilizedMap { map =>
      val result = freshVar("result", BooleanTpe, q"false")
      val body = iterateHashWhile(
          map,
          q"!${result.symbol}",
          idx =>
            q"${result.symbol} = ${app(p, q"$map.keyAt(${idx.symbol})", q"$map.valueAt(${idx.symbol})")}")
      q"""
        $result
        ..$body
        ${result.symbol}
      """
    }
}
