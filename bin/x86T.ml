open Types

type reg = string
type bin_op = 
  | Reg of reg
  | Cst of int
type insn = 
  | Mov of { src: bin_op ; dst: reg }
  | Push of reg
  | LeaFormat of reg
  | Xor of { src: reg ; dst: reg }
  | Call of string
  | Pop of reg
  | Label of { templ: string ; idx: int }
  | Test of reg
  | Jz of { templ: string ; idx: int }
  | Jne of { templ: string ; idx: int }
  | Jmp of { templ: string ; idx: int }
  | Bin_Arithm of { ins: binop ; snd: bin_op }
  | Cmp of { fst: reg ; snd: bin_op }
  | Neg of reg
  | Not of reg
  | Sete of reg
  | Setc of reg
  | Setz of reg
  | Setg of reg
  | Movzx of { src: reg ; dst: reg }