open X86T

let rec opt_movs = function
  | Mov { src = Cst i ; dst = "rax" } :: Mov { src = Reg "rax" ; dst = reg } :: t ->
      opt_movs (Mov { src = Cst i ; dst = reg } :: t)
  | Mov { src = Reg r ; dst = "rax" } :: Cmp { fst = "rax" ; snd = other } :: t ->
      Cmp { fst = r ; snd = other } :: opt_movs t
  | Movzx { src = "al" ; dst = "rax" } :: Mov { src = Reg "rax" ; dst = reg } :: t ->
      opt_movs (Movzx { src = "al" ; dst = reg } :: t)
  | Mov { src = Reg r1 ; dst = "rax" } :: Mov { src = Reg "rax" ; dst = r4 } :: t ->
      opt_movs (Mov { src = Reg r1 ; dst = r4 } :: t)
  | h::t -> h::opt_movs t
  | [] -> []

let execute = opt_movs