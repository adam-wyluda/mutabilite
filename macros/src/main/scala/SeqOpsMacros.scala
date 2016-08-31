package mutabilite.macros

import scala.reflect.macros.whitebox

class SeqOpsMacros(val c: whitebox.Context) extends Common {

  import c.universe._
  import c.universe.definitions._

  def stabilizedSeq(f: Tree => Tree): Tree =
    stabilized(unapplyValueClass(c.prefix.tree)) { seq =>
      q"""
        ..${f(q"${seq.symbol}")}
      """
    }

  def map[B: WeakTypeTag](f: Tree) =
    stabilizedSeq { seq =>
      val arrTpe =
        if (weakTypeOf[B] <:< AnyRefTpe) weakTypeOf[Array[AnyRef]]
        else weakTypeOf[Array[B]]
      val resultTpe = seqType[B]
      val arr = freshVal("array", arrTpe, q"new $arrTpe($seq.size)")
      val body = iterateSeq(
          seq,
          idx => q"${arr.symbol}($idx) = ${app(f, q"$seq($idx)")}"
      )
      q"""
        $arr
        ..$body
        new $resultTpe(${arr.symbol}, $seq.size)
      """
    }

  def flatMap[B: WeakTypeTag](f: Tree) =
    stabilizedSeq { seq =>
      val builderTpe = seqType[B]
      val builder = freshVal("builder",
                             builderTpe,
                             q"new $builderTpe(initialSize = $seq.capacity)")
      val body = iterateSeq(seq, idx => {
        val result =
          freshVal("result", builderTpe, q"${app(f, q"$seq($idx)")}")
        val inner = iterateSeq(
            q"${result.symbol}",
            idx2 => q"${builder.symbol}.append(${result.symbol}($idx2))"
        )
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
    stabilizedSeq { seq =>
      val A = weakTypeOf[A]
      val builderTpe = seqType[A]
      val builder = freshVal("builder", builderTpe, q"new $builderTpe")
      val body = iterateSeq(seq, idx => {
        val el = freshVal("el", A, q"$seq($idx)")
        q"""
          $el
          if (${app(f, q"${el.symbol}")}) ${builder.symbol}.append(${el.symbol})
        """
      })
      q"""
        $builder
        ..$body
        ${builder.symbol}
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

  def zipToMap[A: WeakTypeTag, B: WeakTypeTag](values: Tree) =
    stabilizedSeq { keys =>
      stabilized(values) { values =>
        val idx = freshVar("i", IntTpe, q"0")
        val size = freshVal("size", IntTpe, q"$keys.size min $values.size")
        val builderTpe = mapType[A, B]
        val builder = freshVal(
            "builder",
            builderTpe,
            q"new $builderTpe(initialSize = $keys.capacity min $values.capacity)")
        q"""
          $idx
          $size
          $builder
          while (${idx.symbol} < ${size.symbol}) {
            ${builder.symbol}.put($keys(${idx.symbol}), $values(${idx.symbol}))
            ${idx.symbol} += 1
          }
          ${builder.symbol}
        """
      }
    }

  def groupBy[A: WeakTypeTag, K: WeakTypeTag](f: Tree) =
    stabilizedSeq { seq =>
      val A = weakTypeOf[A]
      val K = weakTypeOf[K]
      val valTpe = seqType[A]
      val bufferTpe = seqType[A]
      val builderTpe = mapTypeSeq[K, A]
      val builder = freshVal(
          "builder",
          builderTpe,
          q"new $builderTpe"
      )
      val body = iterateSeq(seq, idx => {
        val el = freshVal("el", A, q"$seq($idx)")
        val key = freshVal("key", K, q"${app(f, q"${el.symbol}")}")
        val optName = fresh("opt")
        val newSeq = freshVal("seq", valTpe, q"new $bufferTpe")
        var seqVal = freshVal("seq",
                              valTpe,
                              q"""
                                if ($optName.nonEmpty) $optName.get
                                else {
                                  $newSeq
                                  ${builder.symbol}.put(${key.symbol}, ${newSeq.symbol})
                                  ${newSeq.symbol}
                                }
                              """)
        q"""
          $el
          $key
          val $optName = ${builder.symbol}.get(${key.symbol})
          $seqVal
          ${seqVal.symbol}.append(${el.symbol})
        """
      })
      q"""
        $builder
        ..$body
        ${builder.symbol}
      """
    }
}
