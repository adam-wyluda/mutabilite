package mutabilite.macros

import scala.reflect.macros.blackbox

class SeqOpsMacros(val c: blackbox.Context) extends Common {

  import c.universe._
  import c.universe.definitions._

  def stabilizedSeq(f: Tree => Tree): Tree =
    stabilized(unapplyValueClass(c.prefix.tree)) { seq =>
      q"""
        ..${f(q"${seq.symbol}")}
      """
    }

  def foreach(f: Tree) =
    stabilizedSeq { seq =>
      iterateSeq(seq, idx => app(f, q"$seq($idx)"))
    }

  def foldLeft[B: WeakTypeTag](z: Tree)(op: Tree) =
    stabilizedSeq { seq =>
      val accTpe = weakTypeOf[B]
      val acc = freshVar("acc", accTpe, q"$z")
      val body = iterateSeq(
          seq,
          idx =>
            q"${acc.symbol} = ${app(op, q"${acc.symbol}", q"$seq($idx)")}")
      q"""
        $acc
        ..$body
        ${acc.symbol}
      """
    }

  def foldRight[B: WeakTypeTag](z: Tree)(op: Tree) =
    stabilizedSeq { seq =>
      val accTpe = weakTypeOf[B]
      val acc = freshVar("acc", accTpe, q"$z")
      val body = iterateSeqReverse(
          seq,
          idx =>
            q"${acc.symbol} = ${app(op, q"$seq($idx)", q"${acc.symbol}")}")
      q"""
        $acc
        ..$body
        ${acc.symbol}
      """
    }

  def reduceLeft[A: WeakTypeTag](op: Tree) =
    stabilizedSeq { seq =>
      val accTpe = weakTypeOf[A]
      val acc = freshVar("acc", accTpe, q"$seq(0)")
      val idx = freshVar("i", IntTpe, q"1")
      val size = freshVal("size", IntTpe, q"$seq.size")
      q"""
        $acc
        $idx
        $size
        if (${size.symbol} == 0) ${throwUnsupportedOperation("empty.reduce")}
        while (${idx.symbol} < ${size.symbol}) {
          ${acc.symbol} = ${app(op,
                                q"${acc.symbol}",
                                q"${seq.symbol}(${idx.symbol})")}
          ${idx.symbol} += 1
        }
        ${acc.symbol}
      """
    }

  def reduceRight[A: WeakTypeTag](op: Tree) =
    stabilizedSeq { seq =>
      val accTpe = weakTypeOf[A]
      val acc = freshVar("acc", accTpe, q"$seq($seq.size - 1)")
      val idx = freshVar("i", IntTpe, q"$seq.size - 2")
      q"""
        $acc
        $idx
        if ($seq.size == 0) ${throwUnsupportedOperation("empty.reduce")}
        while (${idx.symbol} >= 0) {
          ${acc.symbol} = ${app(op,
                                q"${seq.symbol}(${idx.symbol})",
                                q"${acc.symbol}")}
          ${idx.symbol} -= 1
        }
        ${acc.symbol}
      """
    }

  def transform(f: Tree) =
    stabilizedSeq { seq =>
      iterateSeq(seq, idx => q"$seq($idx) = ${app(f, q"$seq($idx)")}")
    }

  def forall(p: Tree) =
    stabilizedSeq { seq =>
      val result = freshVar("result", BooleanTpe, q"true")
      val body = iterateSeqWhile(
          seq,
          q"${result.symbol}",
          idx => q"${result.symbol} = ${app(p, q"$seq(${idx.symbol})")}")
      q"""
        $result
        ..$body
        ${result.symbol}
      """
    }

  def exists(p: Tree) =
    stabilizedSeq { seq =>
      val result = freshVar("result", BooleanTpe, q"false")
      val body = iterateSeqWhile(
          seq,
          q"!${result.symbol}",
          idx => q"${result.symbol} = ${app(p, q"$seq(${idx.symbol})")}")
      q"""
        $result
        ..$body
        ${result.symbol}
      """
    }

  def sameElements(other: Tree) =
    stabilizedSeq { seq =>
      stabilized(other) { other =>
        val result =
          freshVar("result", BooleanTpe, q"$seq.size == $other.size")
        val body = iterateSeqWhile(
            seq,
            q"${result.symbol}",
            idx => q"${result.symbol} = ($seq($idx) == $other($idx))")
        q"""
          $result
          ..$body
          ${result.symbol}
        """
      }
    }
}
