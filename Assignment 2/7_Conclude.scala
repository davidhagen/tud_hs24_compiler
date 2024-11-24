object Conclude {

  import X86int._

  def concludeProgram(p: X86int): X86int = p match {
    case X86IntProgram(info@ProgramInfo(_, stackSpace), ("start", Blk(inf, instrs)) :: Nil) =>
      X86IntProgram(info, List(
        ("main", Blk(new BlockInfo(), List(
          Instr(Pushq(), List(Reg(RBP()))),
          Instr(Movq(), List(Reg(RSP()), Reg(RBP()))),
          Instr(Subq(), List(Imm(stackSpace + 8), Reg(RSP()))),
          Jmp("start")
        ))),
        ("start", Blk(inf, instrs :+ Jmp("conclusion"))),
        ("conclusion", Blk(new BlockInfo(), List(
          Instr(Addq(), List(Imm(stackSpace + 8), Reg(RSP()))),
          Instr(Popq(), List(Reg(RBP()))),
          Retq()
        )))
      ))
  }
}
