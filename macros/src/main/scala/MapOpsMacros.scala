package offheap.collection.macros

import scala.reflect.macros.whitebox

class MapOpsMacros(val c: whitebox.Context) extends Common {

  import c.universe._
  import c.universe.definitions._

  def map[K: WeakTypeTag, V: WeakTypeTag, T: WeakTypeTag](f: Tree) =
    stabilized(c.prefix.tree) { pre =>
      val mapTpe = mapType[K, V]
      val builderTpe = bufferType[T]
      val idx = freshVar("i", IntTpe, q"0")
      val map = freshVal("map", mapTpe, q"$pre.map")
      val size = freshVal("size", IntTpe, q"${map.symbol}.capacity")
      val builder =
        freshVal("builder",
                 builderTpe,
                 q"new $builderTpe(initialSize = ${map.symbol}.capacity)")
      q"""
        $idx
        $map
        $size
        $builder
        while (${idx.symbol} < ${size.symbol}) {
          if (!${map.symbol}.isInit(${map.symbol}.hashAt(${idx.symbol}))) {
            ${builder.symbol}.append(
              ${app(f,
                    q"${map.symbol}.keyAt(${idx.symbol})",
                    q"${map.symbol}.valueAt(${idx.symbol})")})
          }
          ${idx.symbol} += 1
        }
        ${builder.symbol}
      """
    }

  def mapKeys[K: WeakTypeTag, V: WeakTypeTag, T: WeakTypeTag](f: Tree) =
    stabilized(c.prefix.tree) { pre =>
      val mapTpe = mapType[K, V]
      val builderTpe = hashMapType[T, V]
      val idx = freshVar("i", IntTpe, q"0")
      val map = freshVal("map", mapTpe, q"$pre.map")
      val size = freshVal("size", IntTpe, q"${map.symbol}.capacity")
      val builder =
        freshVal("builder",
                 builderTpe,
                 q"new $builderTpe(initialSize = ${map.symbol}.capacity)")
      q"""
        $idx
        $map
        $size
        $builder
        while (${idx.symbol} < ${size.symbol}) {
          if (!${map.symbol}.isInit(${map.symbol}.hashAt(${idx.symbol}))) {
            ${builder.symbol}.put(
              ${app(f, q"${map.symbol}.keyAt(${idx.symbol})")},
              ${map.symbol}.valueAt(${idx.symbol}))
          }
          ${idx.symbol} += 1
        }
        ${builder.symbol}
      """
    }

  def mapValues[K: WeakTypeTag, V: WeakTypeTag, T: WeakTypeTag](f: Tree) =
    stabilized(c.prefix.tree) { pre =>
      val mapTpe = mapType[K, V]
      val builderTpe = hashMapType[K, T]
      val idx = freshVar("i", IntTpe, q"0")
      val map = freshVal("map", mapTpe, q"$pre.map")
      val size = freshVal("size", IntTpe, q"${map.symbol}.capacity")
      val builder =
        freshVal("builder",
                 builderTpe,
                 q"new $builderTpe(initialSize = ${map.symbol}.capacity)")
      q"""
        $idx
        $map
        $size
        $builder
        while (${idx.symbol} < ${size.symbol}) {
          if (!${map.symbol}.isInit(${map.symbol}.hashAt(${idx.symbol}))) {
            ${builder.symbol}.put(
              ${map.symbol}.keyAt(${idx.symbol}),
              ${app(f, q"${map.symbol}.valueAt(${idx.symbol})")})
          }
          ${idx.symbol} += 1
        }
        ${builder.symbol}
      """
    }

  def flatMap[K: WeakTypeTag, V: WeakTypeTag, T: WeakTypeTag](f: Tree) =
    stabilized(c.prefix.tree) { pre =>
      val mapTpe = mapType[K, V]
      val resultTpe = seqType[T]
      val builderTpe = bufferType[T]
      val idx = freshVar("i", IntTpe, q"0")
      val map = freshVal("map", mapTpe, q"$pre.map")
      val size = freshVal("size", IntTpe, q"${map.symbol}.capacity")
      val builder = freshVal("builder", resultTpe, q"new $builderTpe")
      val result = freshVal(
          "result",
          resultTpe,
          q"${app(f, q"${map.symbol}.keyAt(${idx.symbol})", q"${map.symbol}.valueAt(${idx.symbol})")}")
      val resultSize = freshVal("resultSize", IntTpe, q"${result.symbol}.size")
      val idx2 = freshVar("j", IntTpe, q"0")
      q"""
        $idx
        $map
        $size
        $builder
        while (${idx.symbol} < ${size.symbol}) {
          if (!${map.symbol}.isInit(${map.symbol}.hashAt(${idx.symbol}))) {
            $result
            $resultSize
            $idx2
            while (${idx2.symbol} < ${resultSize.symbol}) {
              ${builder.symbol}.append(${result.symbol}(${idx2.symbol}))
              ${idx2.symbol} += 1
            }
          }
          ${idx.symbol} += 1
        }
        ${builder.symbol}
      """
    }

  def filter[K: WeakTypeTag, V: WeakTypeTag](f: Tree) =
    stabilized(c.prefix.tree) { pre =>
      val K = weakTypeOf[K]
      val V = weakTypeOf[V]
      val mapTpe = mapType[K, V]
      val builderTpe = hashMapType[K, V]
      val idx = freshVar("i", IntTpe, q"0")
      val map = freshVal("map", mapTpe, q"$pre.map")
      val size = freshVal("size", IntTpe, q"${map.symbol}.capacity")
      val builder = freshVal("builder", builderTpe, q"new $builderTpe")
      val key = freshVal("key", K, q"${map.symbol}.keyAt(${idx.symbol})")
      val value = freshVal("value", V, q"${map.symbol}.valueAt(${idx.symbol})")
      q"""
        $idx
        $map
        $size
        $builder
        while (${idx.symbol} < ${size.symbol}) {
          if (!${map.symbol}.isInit(${map.symbol}.hashAt(${idx.symbol}))) {
            $key
            $value
            if (${app(f, q"${key.symbol}", q"${value.symbol}")})
              ${builder.symbol}.put(${key.symbol}, ${value.symbol})
          }
          ${idx.symbol} += 1
        }
        ${builder.symbol}
      """
    }
}
