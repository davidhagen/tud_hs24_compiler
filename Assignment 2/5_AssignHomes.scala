object AssignHomes {

  import X86int._
  import X86var._

  def assignProgram(p: X86var): X86int = p match {
    case X86var.X86VarProgram(_, blocks) => {
      val listOfBlocks = blocks.map(_._2)
      val allInstructions = listOfBlocks.flatMap(x => x match {
        case Blk(_, instrs) => instrs
      })
      val varList = createVarList(allInstructions) // is this correct? do we want a varlist for all blocks?
      X86int.X86IntProgram(ProgramInfo(varList, calcStackSpace(varList)), convertProgram(blocks, varList))
    }
    
  }

  def createVarList(iList: List[Instruction], acc: List[String] = Nil): List[String] = iList match {
    case Nil => acc
    case i :: is => i match {
      // new variables must always be introduced in the second argument in X86var
      case Instr(c, List(_, XVar(x))) => {
        if (!acc.contains(x)) createVarList(is, x :: acc)
        else createVarList(is, acc)
      }
      case _ => createVarList(is, acc)
    }
  }

  def calcStackSpace(varList: List[String]): Int = varList.length * 8

  def convertArg(arg: Arg, varList: List[String]): Arg = arg match {
    case XVar(x) => Deref(RBP(), -(varList.indexOf(x) * 8 + 8))
    case _ => arg
  }

  def convertBlock(iList: List[Instruction], varList: List[String]): List[Instruction] = iList match {
    case Nil => Nil
    case i :: is => i match {
      case Instr(c, List(arg1, arg2)) => Instr(c, List(convertArg(arg1, varList), convertArg(arg2, varList))) :: convertBlock(is, varList)
      case Instr(c, List(arg1)) => Instr(c, List(convertArg(arg1, varList))) :: convertBlock(is, varList)
      case _ => i :: convertBlock(is, varList)
    }
  }

  def convertProgram(blocks: List[(String, Block)], varList: List[String]): List[(String, Block)] = {
    blocks.map { case (s, b) =>
      b match {
        case Blk(bs, instrs) => (s, Blk(bs, convertBlock(instrs, varList)))
      }
    }
  }
}
