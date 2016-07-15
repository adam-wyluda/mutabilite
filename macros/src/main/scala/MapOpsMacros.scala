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
}
