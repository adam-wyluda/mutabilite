package offheap.collection.macros

import scala.reflect.macros.whitebox

// Uses code from:
// https://github.com/densh/scala-offheap
trait Common extends Definitions {

  import c.universe._
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

  def app(f: Tree, argValues: Tree*) = f match {
    case q"(..$params => $body)" =>
      changeOwner(body, f.symbol, enclosingOwner)
      val args = params zip(argValues) map { case (param, argValue) =>
        val q"$_ val $_: $argTpt = $_" = param
        argValue match {
          case rt: RefTree if isSemiStable(rt.symbol) =>
            (rt, q"")
          case _ =>
            val vd = freshVal("arg", argTpt.tpe, argValue)
            (q"${vd.symbol}", vd)
        }
      }
      val argVals = args map (_._1)
      val argDefs = args map (_._2) filter { case q"" => false; case _ => true }

      val param2Val = params zip (argVals) map { case (param, arg) =>
        (param.symbol, arg)
      } toMap

      val transformedBody = typingTransform(body) { (tree, api) =>
        tree match {
          case id: Ident if param2Val.contains(id.symbol) =>
            val arg = param2Val(id.symbol)
            api.typecheck(q"$arg")
          case _ =>
            api.default(tree)
        }
      }

      q"..$argDefs; $transformedBody"
    case _             =>
      q"$f(..$argValues)"
  }
}
