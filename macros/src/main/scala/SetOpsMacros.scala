package offheap.collection.macros

import scala.reflect.macros.whitebox

class SetOpsMacros(val c: whitebox.Context) extends Common {

  import c.universe._
  import c.universe.definitions._

  def stabilizedSet[A: WeakTypeTag](f: Tree => Tree): Tree =
    stabilized(c.prefix.tree) { pre =>
      val setTpe = setType[A]
      val set = freshVal("seq", setTpe, q"$pre.set")
      q"""
        $set
        ..${f(q"${set.symbol}")}
      """
    }

  def map[A: WeakTypeTag, B: WeakTypeTag](f: Tree) =
    stabilizedSet[A] { set =>
      val builderTpe = hashSetType[B]
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

  def flatMap[A: WeakTypeTag, B: WeakTypeTag](f: Tree) =
    stabilizedSet[A] { set =>
      val resultTpe = setType[B]
      val builderTpe = hashSetType[B]
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
    stabilizedSet[A] { set =>
      val A = weakTypeOf[A]
      val builderTpe = hashSetType[A]
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

  def foreach[A: WeakTypeTag](f: Tree) =
    stabilizedSet[A] { set =>
      val idx = freshVar("i", IntTpe, q"0")
      val body = iterateHash(set, idx => q"${app(f, q"$set.keyAt($idx)")}")
      q"""
        ..$body
      """
    }
}
