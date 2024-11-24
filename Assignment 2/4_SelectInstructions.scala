object Select {

  import Lint.{Num, Expr, Prim, Minus, Plus, Read, PrimOp}
  import Lvar._
  import Cvar._
  import X86int._
  import X86var._

  def selectProgram(p: Cvar): X86var = p match {
    case CProgram(blocks) => X86var.X86VarProgram(new Info, blocks.map({
      case (x, t) => (x, Blk(new BlockInfo, selectTail(t))) }))
  }

  def selectAtom(a: Expr): Arg = a match {
    case Num(i) => Imm(i)
    case Var(x) => XVar(x)
    case _ => throw new IllegalArgumentException("Found non-atomic statement")
  }

  def selectStatement(s: Stmt): List[Instruction] = s match {
    case Assign(x, e) => e match {
      case Var(_) | Num(_) => List(Instr(Movq(), List(selectAtom(e), XVar(x))))
      case Prim(Plus(), List(e1, e2)) => (e1, e2) match {
        case (Var(y), j) if y == x => List(Instr(Addq(), List(selectAtom(j), XVar(x)))) // we can do it in only one instruction
        case (j, Var(y)) if y == x => List(Instr(Addq(), List(selectAtom(j), XVar(x)))) // we can do it in only one instruction
        case (i, j) => List(Instr(Movq(), List(selectAtom(i), XVar(x))), Instr(Addq(), List(selectAtom(j), XVar(x)))) // we need two instructions to add
      }
      case Prim(Minus(), List(e1, e2)) => (e1, e2) match {
        case (Var(y), j) if y == x => List(Instr(Subq(), List(selectAtom(j), XVar(x)))) // we can do it in only one instruction
        case (j, Var(y)) if y == x => List(Instr(Negq(), List(XVar(x))), Instr(Addq(), List(selectAtom(j), XVar(x)))) // negq x; addq j x => is this case even possible in Cvar?
        case (i, j) => List(Instr(Movq(), List(selectAtom(i), XVar(x))), Instr(Subq(), List(selectAtom(j), XVar(x)))) // we need two instructions to subtract
      }
      case Prim(Minus(), List(e1)) => List(Instr(Movq(), List(selectAtom(e1), XVar(x))), Instr(Negq(), List(XVar(x))))
      case Prim(Read(), List()) => List(Callq("_read_int", 0), Instr(Movq(), List(Reg(RAX()), XVar(x))))
      case Prim(Print(), List(e1)) => List(Instr(Movq(), List(selectAtom(e1), Reg(RDI()))), Callq("_print_int", 1), Instr(Movq(), List(Reg(RAX()), XVar(x))))
    }
  }

  def selectTail(t: Tail): List[Instruction] = t match {
    case Return(e) => e match {
      case Var(_) | Num(_) => List(Instr(Movq(), List(selectAtom(e), Reg(RAX()))))
      case Prim(Plus(), List(e1, e2)) => List(Instr(Movq(), List(selectAtom(e1), Reg(RAX()))), Instr(Addq(), List(selectAtom(e2), Reg(RAX()))))
      case Prim(Minus(), List(e1, e2)) => List(Instr(Movq(), List(selectAtom(e1), Reg(RAX()))), Instr(Subq(), List(selectAtom(e2), Reg(RAX()))))
      case Prim(Minus(), List(e1)) => List(Instr(Movq(), List(selectAtom(e1), Reg(RAX()))), Instr(Negq(), List(Reg(RAX()))))
      case Prim(Read(), List()) => List(Callq("_read_int", 0))
      case Prim(Print(), List(e1)) => List(Instr(Movq(), List(selectAtom(e1), Reg(RDI()))), Callq("_print_int", 1))
    }
    case Seq(s, ts) => selectStatement(s) ::: selectTail(ts)
  }
}
