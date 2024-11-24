object RemoveComplexOperands {

  import Lint.{Num, Expr, Prim, Minus, Plus, Read, PrimOp}
  import Lvar._
  import GenSym._
  
  abstract class Atom
  case class I(i: Long) extends Atom
  case class X(x: String) extends Atom

  def rcoProgram(p: Lvar): Lvar = p match {
    case Program(body) => Program(rcoExpression(body))
  }

  def rcoExpression(e: Expr): Expr = e match {
    case Var(x) => Var(x)
    case Num(i) => Num(i)
    case Prim(op, es) => listToLet(rcoAtom(Prim(op, es)))
    case Let(x, e1, e2) => Let(x, rcoExpression(e1), rcoExpression(e2))
  }

  def listToLet(a: (Atom, List[(String, Expr)])): Expr = a match {
    case (I(i), Nil) => Num(i)
    case (X(x), Nil) => Var(x)
    case (q, x :: xs) => Let(x._1, x._2, listToLet(q, xs))
  }

  def rcoAtom(e: Expr): (Atom, List[(String, Expr)]) = e match {
    case Var(x) => (X(x), Nil)
    case Num(i) => (I(i), Nil)
    case Prim(op, es) => {
      val rec_result = es.map(e => {
        val (tmpAtom, list) = rcoAtom(e)
        val atomConv = atomConversion(tmpAtom)
        (atomConv, list)
      })
      val new_var = gensym("tmp")
      val unzipped = rec_result.unzip
      (X(new_var), unzipped._2.flatten :+ (new_var, Prim(op, unzipped._1)))
    }
    case Let(x, e1, e2) => {
      val simple_e1 = rcoExpression(e1)
      val rec_e2 = rcoAtom(e2)
      (rec_e2._1, (x, simple_e1) :: rec_e2._2)
    }
  }

  def atomConversion(a: Atom): Expr = a match {
    case I(i) => Num(i)
    case X(x) => Var(x)
  }
  
}
