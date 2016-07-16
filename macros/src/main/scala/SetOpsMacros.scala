package offheap.collection.macros

import scala.reflect.macros.whitebox

class SetOpsMacros(val c: whitebox.Context) extends Common {

  import c.universe._
  import c.universe.definitions._

  def map[A: WeakTypeTag, B: WeakTypeTag](f: Tree) =
    stabilized(c.prefix.tree) { pre =>
      val setTpe = setType[A]
      val builderTpe = hashSetType[B]
      val set = freshVal("set", setTpe, q"$pre.set")
      val builder =
        freshVal("builder",
                 builderTpe,
                 q"new $builderTpe(initialSize = ${set.symbol}.capacity)")
      val body = iterateHash(
          q"${set.symbol}",
          idx =>
            q"${builder.symbol}.add(${app(f, q"${set.symbol}.keyAt(${idx.symbol})")})"
      )
      q"""
        $set
        $builder
        ..$body
        ${builder.symbol}
      """
    }

  def flatMap[A: WeakTypeTag, B: WeakTypeTag](f: Tree) =
    stabilized(c.prefix.tree) { pre =>
      val setTpe = setType[A]
      val resultTpe = setType[B]
      val builderTpe = hashSetType[B]
      val set = freshVal("set", setTpe, q"$pre.set")
      val builder = freshVal("builder", builderTpe, q"new $builderTpe")
      val body = iterateHash(q"${set.symbol}", idx => {
        val result = freshVal("result",
                              resultTpe,
                              q"${app(f, q"${set.symbol}.keyAt($idx)")}")
        val inner = iterateHash(
            q"${result.symbol}",
            idx2 => q"${builder.symbol}.add(${result.symbol}.keyAt($idx2))")
        q"""
            $result
            ..$inner
          """
      })
      q"""
        $set
        $builder
        ..$body
        ${builder.symbol}
      """
    }

  def filter[A: WeakTypeTag](f: Tree) =
    stabilized(c.prefix.tree) { pre =>
      val A = weakTypeOf[A]
      val setTpe = setType[A]
      val builderTpe = hashSetType[A]
      val set = freshVal("set", setTpe, q"$pre.set")
      val builder = freshVal("builder", builderTpe, q"new $builderTpe")
      val body = iterateHash(q"${set.symbol}", idx => {
        val el = freshVal("el", A, q"${set.symbol}.keyAt($idx)")
        q"""
          $el
          if (${app(f, q"${el.symbol}")}) ${builder.symbol}.add(${el.symbol})
        """
      })
      q"""
        $set
        $builder
        ..$body
        ${builder.symbol}
      """
    }

  def foreach[A: WeakTypeTag](f: Tree) =
    stabilized(c.prefix.tree) { pre =>
      val idx = freshVar("i", IntTpe, q"0")
      val setTpe = setType[A]
      val set = freshVal("set", setTpe, q"$pre.set")
      val body = iterateHash(q"${set.symbol}",
                             idx => q"${app(f, q"${set.symbol}.keyAt($idx)")}")
      q"""
        $set
        ..$body
      """
    }
}
