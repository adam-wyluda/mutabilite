package mutabilite.macros

import scala.reflect.macros.whitebox

class SetOpsMacrosWhitebox(override val c: whitebox.Context) extends SetOpsMacros(c) {

  import c.universe._

  def map[B: WeakTypeTag](f: Tree) =
    stabilizedSet { set =>
      val builderTpe = setType[B]
      val builder = freshVal("builder",
        builderTpe,
        q"new $builderTpe(initialSize = $set.capacity)")
      val body = iterateHash(
        set,
        idx =>
          q"${builder.symbol}.add(${app(f, q"$set.keyAt(${idx.symbol})")})"
      )
      q"""
        $builder
        ..$body
        ${builder.symbol}
      """
    }

  def flatMap[B: WeakTypeTag](f: Tree) =
    stabilizedSet { set =>
      val resultTpe = setType[B]
      val builderTpe = setType[B]
      val builder = freshVal("builder", builderTpe, q"new $builderTpe")
      val body = iterateHash(set, idx => {
        val result =
          freshVal("result", resultTpe, q"${app(f, q"$set.keyAt($idx)")}")
        val inner = iterateHash(
          q"${result.symbol}",
          idx2 => q"${builder.symbol}.add(${result.symbol}.keyAt($idx2))")
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

  def filter[A: WeakTypeTag](f: Tree) =
    stabilizedSet { set =>
      val A = weakTypeOf[A]
      val builderTpe = setType[A]
      val builder = freshVal("builder", builderTpe, q"new $builderTpe")
      val body = iterateHash(set, idx => {
        val el = freshVal("el", A, q"$set.keyAt($idx)")
        q"""
          $el
          if (${app(f, q"${el.symbol}")}) ${builder.symbol}.add(${el.symbol})
        """
      })
      q"""
        $builder
        ..$body
        ${builder.symbol}
      """
    }
}
