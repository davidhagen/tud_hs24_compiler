object Uniquify {
  
  import Lint.{Num, Expr, Prim, Minus, Plus, Read, PrimOp}
  import Lvar._
  import GenSym._

  def uniquifyProgram(p: Lvar): Lvar = p match {
    case Program(body) => Program(uniquifyExpression(body, Map.empty[String, String]))
  }

  def uniquifyExpression(e: Expr, env: Map[String, String]): Expr = e match {
    case Num(i) => Num(i)
    case Var(x) => Var(env(x)) // match variable name
    case Prim(x, es) => Prim(x, es.map(e => uniquifyExpression(e, env)))
    case Let(x, e1, e2) => {
      // create new environment with shadowing variable
      val new_x = gensym(x);
      val new_env = env + (x -> new_x)
      // recursively uniquify with new environment in body, old environment in var assignment
      Let(new_x, uniquifyExpression(e1, env), uniquifyExpression(e2, new_env))
    }
  }

}
