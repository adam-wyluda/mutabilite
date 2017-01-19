package mutabilite.macros

import scala.reflect.macros.blackbox

class MapOpsMacros(val c: blackbox.Context) extends Common {

  import c.universe._
  import c.universe.definitions._

  def stabilizedMap(f: Tree => Tree): Tree =
    stabilized(unapplyValueClass(c.prefix.tree)) { map =>
      q"""
        ..${f(q"${map.symbol}")}
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
