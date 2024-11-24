object PatchInstructions {

  import X86int._

  def patchProgram(p: X86int): X86int = p match {
    case X86IntProgram(info, blocks) => X86IntProgram(info, blocks.map { case (s, b) => 
      b match {
        case Blk(i, instrs) => (s, Blk(i, convertBlock(instrs)))
      }
    })
  }

  // when we detect an instruction with two memory arguments, move the first one to rax and then perform the operation with rax
  def convertBlock(iList: List[Instruction]): List[Instruction] = iList match {
    case Nil => Nil
    case i :: is => i match {
      case Instr(op, List(Deref(r1, i1), Deref(r2, i2))) => {
        Instr(Movq(), List(Deref(r1, i1), Reg(RAX()))) ::
        Instr(op, List(Reg(RAX()), Deref(r2, i2))) :: convertBlock(is)
      }
      case _ => i :: convertBlock(is)
    }
  }

}
