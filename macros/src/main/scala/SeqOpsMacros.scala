package offheap.collection.macros

import scala.reflect.macros.whitebox

class SeqOpsMacros(val c: whitebox.Context) extends Common {

  import c.universe._
  import c.universe.definitions._

  def seqType[T: WeakTypeTag]: Type =
    SeqClass(typeName[T]).asType.toType

  def bufferType[T: WeakTypeTag]: Type =
    BufferSeqClass(typeName[T]).asType.toType

  def map[A: WeakTypeTag, B: WeakTypeTag](f: Tree) =
    stabilized(c.prefix.tree) { pre =>
      val seqTpe = seqType[A]
      val builderTpe = bufferType[B]
      val idx = freshVar("i", IntTpe, q"0")
      val seq = freshVal("seq", seqTpe, q"$pre.seq")
      val size = freshVal("size", IntTpe, q"${seq.symbol}.size")
      val builder =
        freshVal("builder",
                 builderTpe,
                 q"new $builderTpe(initialSize = ${seq.symbol}.capacity)")
      q"""
        $idx
        $seq
        $size
        $builder
        while (${idx.symbol} < ${size.symbol}) {
          ${builder.symbol}.append(${app(f, q"${seq.symbol}(${idx.symbol})")})
          ${idx.symbol} += 1
        }
        ${builder.symbol}
      """
    }

  def flatMap[A: WeakTypeTag, B: WeakTypeTag](f: Tree) =
    stabilized(c.prefix.tree) { pre =>
      val A = weakTypeOf[A]
      val seqTpe = seqType[A]
      val builderTpe = bufferType[B]
      val idx = freshVar("i", IntTpe, q"0")
      val seq = freshVal("seq", seqTpe, q"$pre.seq")
      val size = freshVal("size", IntTpe, q"${seq.symbol}.size")
      val builder =
        freshVal("builder",
                 builderTpe,
                 q"new $builderTpe(initialSize = ${seq.symbol}.capacity)")
      val el = freshVal("el", A, q"${seq.symbol}(${idx.symbol})")
      val result =
        freshVal("result", builderTpe, q"${app(f, q"${el.symbol}")}")
      val resultSize = freshVal("resultSize", IntTpe, q"${result.symbol}.size")
      val idx2 = freshVar("j", IntTpe, q"0")
      q"""
        $idx
        $seq
        $size
        $builder
        while (${idx.symbol} < ${size.symbol}) {
          $el
          $result
          $resultSize
          $idx2
          while (${idx2.symbol} < ${resultSize.symbol}) {
            ${builder.symbol}.append(${result.symbol}(${idx2.symbol}))
            ${idx2.symbol} += 1
          }
          ${idx.symbol} += 1
        }
        ${builder.symbol}
      """
    }

  def filter[A: WeakTypeTag](f: Tree) =
    stabilized(c.prefix.tree) { pre =>
      val A = weakTypeOf[A]
      val seqTpe = seqType[A]
      val builderTpe = bufferType[A]
      val idx = freshVar("i", IntTpe, q"0")
      val seq = freshVal("seq", seqTpe, q"$pre.seq")
      val size = freshVal("size", IntTpe, q"${seq.symbol}.size")
      val builder = freshVal("builder", builderTpe, q"new $builderTpe")
      val el = freshVal("el", A, q"${seq.symbol}(${idx.symbol})")
      q"""
        $idx
        $seq
        $size
        $builder
        while (${idx.symbol} < ${size.symbol}) {
          $el
          if (${app(f, q"${el.symbol}")}) ${builder.symbol}.append(${el.symbol})
          ${idx.symbol} += 1
        }
        ${builder.symbol}
      """
    }
}
