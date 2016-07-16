package offheap.collection.macros

import scala.reflect.macros.whitebox

class SeqOpsMacros(val c: whitebox.Context) extends Common {

  import c.universe._
  import c.universe.definitions._

  def map[A: WeakTypeTag, B: WeakTypeTag](f: Tree) =
    stabilized(c.prefix.tree) { pre =>
      val seqTpe = seqType[A]
      val builderTpe = bufferType[B]
      val seq = freshVal("seq", seqTpe, q"$pre.seq")
      val builder =
        freshVal("builder",
                 builderTpe,
                 q"new $builderTpe(initialSize = ${seq.symbol}.capacity)")
      val body = iterateSeq(
          q"${seq.symbol}",
          idx => q"${builder.symbol}.append(${app(f, q"${seq.symbol}($idx)")})"
      )
      q"""
        $seq
        $builder
        ..$body
        ${builder.symbol}
      """
    }

  def flatMap[A: WeakTypeTag, B: WeakTypeTag](f: Tree) =
    stabilized(c.prefix.tree) { pre =>
      val seqTpe = seqType[A]
      val builderTpe = bufferType[B]
      val seq = freshVal("seq", seqTpe, q"$pre.seq")
      val builder =
        freshVal("builder",
                 builderTpe,
                 q"new $builderTpe(initialSize = ${seq.symbol}.capacity)")
      val body = iterateSeq(q"${seq.symbol}", idx => {
        val result =
          freshVal("result", builderTpe, q"${app(f, q"${seq.symbol}($idx)")}")
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
        $seq
        $builder
        ..$body
        ${builder.symbol}
      """
    }

  def filter[A: WeakTypeTag](f: Tree) =
    stabilized(c.prefix.tree) { pre =>
      val A = weakTypeOf[A]
      val seqTpe = seqType[A]
      val builderTpe = bufferType[A]
      val seq = freshVal("seq", seqTpe, q"$pre.seq")
      val builder = freshVal("builder", builderTpe, q"new $builderTpe")
      val body = iterateSeq(q"${seq.symbol}", idx => {
        val el = freshVal("el", A, q"${seq.symbol}($idx)")
        q"""
          $el
          if (${app(f, q"${el.symbol}")}) ${builder.symbol}.append(${el.symbol})
        """
      })
      q"""
        $seq
        $builder
        ..$body
        ${builder.symbol}
      """
    }

  def foreach[A: WeakTypeTag](f: Tree) =
    stabilized(c.prefix.tree) { pre =>
      val seqTpe = seqType[A]
      val seq = freshVal("seq", seqTpe, q"$pre.seq")
      val body = iterateSeq(q"${seq.symbol}",
                            idx => q"${app(f, q"${seq.symbol}($idx)")}")
      q"""
        $seq
        ..$body
      """
    }
}
