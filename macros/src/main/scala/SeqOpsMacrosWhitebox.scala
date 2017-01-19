package mutabilite.macros

import scala.reflect.macros.whitebox

class SeqOpsMacrosWhitebox(override val c: whitebox.Context) extends SeqOpsMacros(c) {

  import c.universe._
  import c.universe.definitions._

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
