object ExplicateControl {

  import Lint.{Num, Expr, Prim, Minus, Plus, Read, PrimOp}
  import Lvar._
  import Cvar._

  def explicateProgram(p: Lvar): Cvar = p match {
    case Lvar.Program(e) => CProgram(List(("start", explicateTail(e))))
  }

  def explicateTail(e: Expr): Tail = e match {
    case Num(i) => Return(Num(i))
    case Var(x) => Return(Var(x))
    case Let(x, e1, e2) => explicateAssign(e1, x, explicateTail(e2))
    case Prim(op, es) => Return(Prim(op, es))
  }

  def explicateAssign(e: Expr, x: String, t: Tail): Tail = e match {
    case Var(x2) => Seq(Assign(x, Var(x2)), t)
    case Num(i) => Seq(Assign(x, Num(i)), t)
    case Let(x2, e1, e2) => toSeq(assignHelper(x2, e1), explicateAssign(e2, x, t)) // change to list behavior
    case Prim(op, es) => Seq(Assign(x, Prim(op, es)), t)
  }

  def assignHelper(x: String, e: Expr): List[Stmt] = e match {
    case Let(x2, e1, e2) => Assign(x2, e1) :: assignHelper(x, e2)
    case _ => Assign(x, e) :: Nil
  }

  def toSeq(xs: List[Stmt], t: Tail): Tail = xs match {
    case Nil => t
    case x :: xs => Seq(x, toSeq(xs, t))
  }

}
