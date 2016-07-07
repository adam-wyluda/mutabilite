package offheap.collection.macros

import scala.reflect.macros.whitebox

// Uses code from:
// https://github.com/densh/scala-offheap
trait Common {

  val c: whitebox.Context

  import c.universe.{ weakTypeOf => wt, _ }
  import c.universe.definitions._
  import c.internal._, decorators._

  class SemiStable

  def freshVal(pre: String, tpe: Type, value: Tree, flags: FlagSet = NoFlags): ValDef = {
    val name = fresh(pre)
    val sym = enclosingOwner.newTermSymbol(name).setFlag(flags).setInfo(tpe)
    sym.updateAttachment(new SemiStable)
    val vd = valDef(sym, value)
    vd
  }

  def freshVar(pre: String, tpe: Type, value: Tree): ValDef =
    freshVal(pre, tpe, value, flags = Flag.MUTABLE)

  def fresh(pre: String): TermName = TermName(c.freshName(pre))

  def stabilized(tree: Tree)(f: Tree => Tree) = tree match {
    case q"${const: Literal}" =>
      f(const)
    case q"${refTree: RefTree}" if isSemiStable(refTree.symbol) =>
      f(refTree)
    case _ =>
      if (tree.tpe == null) {
        val stable = fresh("stable")
        q"val $stable = $tree; ${f(q"$stable")}"
      } else {
        val stable = freshVal("stable", tree.tpe, tree)
        val fapp = f(q"${stable.symbol}")
        q"$stable; $fapp"
      }
  }

  def isSemiStable(sym: Symbol) =
    (sym.isTerm && sym.asTerm.isStable) || sym.attachments.get[SemiStable].nonEmpty
}
