package mutabilite.macros

import scala.reflect.macros.blackbox

class SetOpsMacros(val c: blackbox.Context) extends Common {

  import c.universe._
  import c.universe.definitions._

  def stabilizedSet(f: Tree => Tree): Tree =
    stabilized(unapplyValueClass(c.prefix.tree)) { set =>
      q"""
        ..${f(q"${set.symbol}")}
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
