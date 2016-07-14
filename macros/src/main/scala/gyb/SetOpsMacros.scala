package offheap.collection.macros

import scala.reflect.macros.whitebox

class SetOpsMacros(val c: whitebox.Context) extends Common {

  import c.universe._
  import c.universe.definitions._

  def map[A: WeakTypeTag, B: WeakTypeTag](f: Tree) =
    stabilized(c.prefix.tree) { pre =>
      val setTpe = setType[A]
      val builderTpe = hashSetType[B]
      val idx = freshVar("i", IntTpe, q"0")
      val set = freshVal("set", setTpe, q"$pre.set")
      val size = freshVal("size", IntTpe, q"${set.symbol}.capacity")
      val builder =
        freshVal("builder",
                 builderTpe,
                 q"new $builderTpe(initialSize = ${set.symbol}.capacity)")
      q"""
        $idx
        $set
        $size
        $builder
        while (${idx.symbol} < ${size.symbol}) {
          if (!${set.symbol}.isInit(${set.symbol}.hashAt(${idx.symbol}))) {
            ${builder.symbol}.add(${app(
          f,
          q"${set.symbol}.keyAt(${idx.symbol})")})
          }
          ${idx.symbol} += 1
        }
        ${builder.symbol}
      """
    }

  def flatMap[A: WeakTypeTag, B: WeakTypeTag](f: Tree) =
    stabilized(c.prefix.tree) { pre =>
      val setTpe = setType[A]
      val builderTpe = hashSetType[B]
      val idx = freshVar("i", IntTpe, q"0")
      val set = freshVal("set", setTpe, q"$pre.set")
      val size = freshVal("size", IntTpe, q"${set.symbol}.capacity")
      val builder = freshVal("builder", builderTpe, q"new $builderTpe")
      val result =
        freshVal("result",
                 builderTpe,
                 q"${app(f, q"${set.symbol}.keyAt(${idx.symbol})")}")
      val resultSize =
        freshVal("resultSize", IntTpe, q"${result.symbol}.capacity")
      val idx2 = freshVar("j", IntTpe, q"0")
      q"""
        $idx
        $set
        $size
        $builder
        while (${idx.symbol} < ${size.symbol}) {
          if (!${set.symbol}.isInit(${set.symbol}.hashAt(${idx.symbol}))) {
            $result
            $resultSize
            $idx2
            while (${idx2.symbol} < ${resultSize.symbol}) {
              if (!${result.symbol}.isInit(${result.symbol}.hashAt(${idx2.symbol}))) {
                ${builder.symbol}.add(${result.symbol}.keyAt(${idx2.symbol}))
              }
              ${idx2.symbol} += 1
            }
          }
          ${idx.symbol} += 1
        }
        ${builder.symbol}
      """
    }

  def filter[A: WeakTypeTag](f: Tree) =
    stabilized(c.prefix.tree) { pre =>
      val A = weakTypeOf[A]
      val setTpe = setType[A]
      val builderTpe = hashSetType[A]
      val idx = freshVar("i", IntTpe, q"0")
      val set = freshVal("set", setTpe, q"$pre.set")
      val size = freshVal("size", IntTpe, q"${set.symbol}.capacity")
      val builder = freshVal("builder", builderTpe, q"new $builderTpe")
      val el = freshVal("el", A, q"${set.symbol}.keyAt(${idx.symbol})")
      q"""
        $idx
        $set
        $size
        $builder
        while (${idx.symbol} < ${size.symbol}) {
          if (!${set.symbol}.isInit(${set.symbol}.hashAt(${idx.symbol}))) {
            $el
            if (${app(f, q"${el.symbol}")}) ${builder.symbol}.add(${el.symbol})
          }
          ${idx.symbol} += 1
        }
        ${builder.symbol}
      """
    }

  def foreach[A: WeakTypeTag](f: Tree) =
    stabilized(c.prefix.tree) { pre =>
      val idx = freshVar("i", IntTpe, q"0")
      val setTpe = setType[A]
      val set = freshVal("set", setTpe, q"$pre.set")
      val size = freshVal("size", IntTpe, q"${set.symbol}.capacity")
      q"""
        $idx
        $set
        $size
        while (${idx.symbol} < ${size.symbol}) {
          if (!${set.symbol}.isInit(${set.symbol}.hashAt(${idx.symbol}))) {
            ${app(f, q"${set.symbol}.keyAt(${idx.symbol})")}
          }
          ${idx.symbol} += 1
        }
      """
    }
}
