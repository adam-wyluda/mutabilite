package offheap.collection.macros

import scala.reflect.macros.whitebox

class SetOpsMacros(val c: whitebox.Context) extends Common {

  import c.universe._
  import c.universe.definitions._

  def stabilizedSet(f: Tree => Tree): Tree =
    stabilized(unapplyValueClass(c.prefix.tree)) { set =>
      q"""
        ..${f(q"${set.symbol}")}
      """
    }

  def map[B: WeakTypeTag](f: Tree) =
    stabilizedSet { set =>
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

  def flatMap[B: WeakTypeTag](f: Tree) =
    stabilizedSet { set =>
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
    stabilizedSet { set =>
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

  def foreach(f: Tree) =
    stabilizedSet { set =>
      iterateHash(set, idx => app(f, q"$set.keyAt($idx)"))
    }

  def fold[B: WeakTypeTag](z: Tree)(op: Tree) =
    stabilizedSet { set =>
      val accTpe = weakTypeOf[B]
      val acc = freshVar("acc", accTpe, q"$z")
      val body = iterateHash(
          set,
          idx =>
            q"${acc.symbol} = ${app(op, q"${acc.symbol}", q"$set.keyAt($idx)")}")
      q"""
        $acc
        ..$body
        ${acc.symbol}
      """
    }

  def reduce[A: WeakTypeTag](op: Tree) =
    stabilizedSet { set =>
      reduceHash[A](set, op, TermName("keyAt"))
    }

  def forall(p: Tree) =
    stabilizedSet { set =>
      val result = freshVar("result", BooleanTpe, q"true")
      val body = iterateHashWhile(
          set,
          q"${result.symbol}",
          idx => q"${result.symbol} = ${app(p, q"$set.keyAt(${idx.symbol})")}")
      q"""
        $result
        ..$body
        ${result.symbol}
      """
    }

  def exists(p: Tree) =
    stabilizedSet { set =>
      val result = freshVar("result", BooleanTpe, q"false")
      val body = iterateHashWhile(
          set,
          q"!${result.symbol}",
          idx => q"${result.symbol} = ${app(p, q"$set.keyAt(${idx.symbol})")}")
      q"""
        $result
        ..$body
        ${result.symbol}
      """
    }
}
