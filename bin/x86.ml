open Types
open X86T

let var_register = function
  | X -> "r8"   | Y -> "r9"   | Z -> "r10"
  | A -> "r11"  | B -> "r12"  | C -> "r13"

let insn_atom = function
  | Avar v -> Mov { src = Reg (var_register v) ; dst = "rax" }
  | Aint i -> Mov { src = Cst i ; dst = "rax" }

let insns_expr expr = 
  let atom_bin_op = function
    | Avar v -> Reg (var_register v)
    | Aint i -> Cst i
  in let insns_eql snd = [
      Cmp { fst = "rax" ; snd = atom_bin_op snd } ;
      Setz "al" ;
      Movzx { src = "al" ; dst = "rax" }
    ]
  and insns_less snd = [
      Cmp { fst = "rax" ; snd = atom_bin_op snd } ;
      Setc "al" ;
      Movzx { src = "al" ; dst = "rax" }
    ]
  and insns_greater i = [
      Cmp { fst = "rax" ; snd = Cst i } ;
      Setg "al" ;
      Movzx { src = "al" ; dst = "rax" }
    ]
  in match expr with 
  | Eatom a -> 
      [ insn_atom a ]
  | Eunop { op = Neg ; arg = Aint i } -> 
      [ insn_atom (Aint (-i)) ]
  | Eunop { op = Neg ; arg = v } -> 
      insn_atom v :: [ Neg "rax" ]
  | Eunop { op = Not ; arg = Aint i } -> 
      [ insn_atom (Aint (if i = 0 then 0 else 1)) ]
  | Eunop { op = Not ; arg = v } -> 
      insn_atom v :: [ Cmp {fst = "rax" ; snd = Cst 0 } ; Sete "al" ; Movzx { src = "al" ; dst = "rax" } ]

  | Ebinop { op = Equal ; left = Aint i ; right = Aint i2 } -> 
      [ insn_atom (Aint (if i = i2 then 1 else 0)) ]
  | Ebinop { op = Equal ; left = Aint v ; right = r } -> 
      insn_atom r :: insns_eql (Aint v)
  | Ebinop { op = Equal ; left = l ; right = r } -> 
      insn_atom l :: insns_eql r

  | Ebinop { op = Lt ; left = Aint i ; right = Aint i2 } ->
      [ insn_atom (Aint (if i < i2 then 1 else 0)) ]
  | Ebinop { op = Lt ; left = Aint i ; right = r } ->
      insn_atom r :: insns_greater i
  | Ebinop { op = Lt ; left = l ; right = r } -> 
      insn_atom l :: insns_less r
  | Ebinop { op = Div ; left = l ; right = r } ->
      insn_atom l :: [ Bin_Arithm { ins = Div ; snd = atom_bin_op r } ]
  | Ebinop { op ; left ; right } -> 
      insn_atom left :: [ Bin_Arithm { ins = op ; snd = atom_bin_op right } ]

let printf reg = [
  Push "r8" ; Push "r9" ; Push "r10" ; Push "r11" ; Push "r12" ; Push "r13" ;
  Mov { src = reg ; dst = "rdi" } ;
  Xor { src = "rax" ; dst = "rax" } ;
  Call "_printf" ;
  Pop "r13" ; Pop "r12" ; Pop "r11" ; Pop "r10" ; Pop "r9" ; Pop "r8"
]

(*   "   lea     rdi, format [rip]" ; *)

let while_pre reg idx = [
  Label { templ = "while" ; idx = idx } ;
  Test reg ;
  Jz { templ = "past_while" ; idx = idx }
]

let while_post idx = [
  Jmp { templ = "while" ; idx = idx } ;
  Label { templ = "past_while" ; idx = idx }
]

let if_pre reg idx = [
  Label { templ = "if" ; idx = idx } ;
  Cmp { fst = reg ; snd = Cst 0 } ; 
  Jz { templ = "else" ; idx = idx } 
]

let else_pre idx = [
  Jmp { templ = "past_else" ; idx = idx } ;
  Label { templ = "else" ; idx = idx }
]

let else_post idx = [
  Label { templ = "past_else" ; idx = idx }
]

let main_pre = [
    "    .intel_syntax noprefix";
    "    .text";
    "    .section    .rodata";
    ".LC0:";
    "    .text";
    "    .globl  main";
    "    .type   main, @function";
    "main:";
    "    .cfi_startproc";
    "    enter 0, 1"
]

let main_post = [
    "    mov   eax, 0";
    "    leave";
    "    ret";
    "    .cfi_endproc";
    ".LFE0:";
    "    .size   main, .-main";
    "    .ident  \"SLC\"";
    "    .section    .note.GNU-stack,\"\",@progbits"
]

let while_idx = ref 0
let if_idx = ref 0

let rec stmts_insns stmts = 
  let stmt_insns = function
    | Sprint v -> 
        printf (Reg (var_register v))
    | Sassign { var ; value } -> 
        List.flatten [
          insns_expr value ;
          [ Mov { src = Reg "rax" ; dst = var_register var } ]
        ]
    | Swhile { condition ; body } -> 
        while_idx := !while_idx + 1 ; 
        List.flatten [
          while_pre (var_register condition) !while_idx ;
          stmts_insns body ;
          while_post !while_idx
        ]
    | Sif { condition ; true_body ; false_body } ->
        if_idx := !if_idx + 1 ;
        List.flatten [
          if_pre (var_register condition) !if_idx ;
          stmts_insns true_body ;
          else_pre !if_idx ;
          stmts_insns false_body ;
          else_post !if_idx
        ]
  in List.flatten (List.map (stmt_insns) stmts)

let insn_mul = function
  | Reg r -> ["    imul  rax, " ^ r]
  | Cst i -> ["    mov   rbx, " ^ string_of_int i ;
              "    imul  rax, rbx"]

let insn_div = function
  | Reg r -> ["    mov   rdx, 0" ;
              "    idiv  " ^ r ]
  | Cst i -> ["    mov   rdx, 0" ;
              "    mov   rbx, " ^ string_of_int i ;
              "    idiv  rbx"]

let insn_bin_arithm op arg = 
  let reg_cst_arg = match arg with Reg r -> r | Cst i -> string_of_int i
  in match op with
  | Add -> ["    add   rax, " ^ reg_cst_arg]
  | Sub -> ["    sub   rax, " ^ reg_cst_arg]
  | Mul -> insn_mul arg
  | Div -> insn_div arg
  | _   -> failwith "Binary  arithmetic: unsupported operator"
  
let insns_str insns = 
  let insn_str insn = 
    let s = string_of_int
    in let fmt t idx = "." ^ t ^ "_" ^ s idx
    in match insn with
    | Mov { src = Reg src ; dst = dst } -> ["    mov   " ^ dst ^ ", " ^ src]
    | Mov { src = Cst i ; dst = dst }   -> ["    mov   " ^ dst ^ ", " ^ s i]
    | Push r                            -> ["    push  " ^ r]
    | LeaFormat r                       -> ["    lea   " ^ r ^ ", format [rip]"]
    | Xor { src ; dst }                 -> ["    xor   " ^ dst ^ ", " ^ src]
    | Call f                            -> ["    call  " ^ f]
    | Pop r                             -> ["    pop   " ^ r]
    | Label { templ ; idx }             -> [fmt templ idx ^ ":"]
    | Test r                            -> ["    test  " ^ r ^ ", " ^ r]
    | Jz { templ ; idx }                -> ["    jz    " ^ fmt templ idx]
    | Jne { templ ; idx }               -> ["    jne   " ^ fmt templ idx]
    | Jmp { templ ; idx }               -> ["    jmp   " ^ fmt templ idx]
    | Cmp { fst = reg ; snd = Reg snd } -> ["    cmp   " ^ reg ^ ", " ^ snd]
    | Cmp { fst = reg ; snd = Cst i }   -> ["    cmp   " ^ reg ^ ", " ^ s i]
    | Neg reg                           -> ["    neg   " ^ reg]
    | Not reg                           -> ["    not   " ^ reg]
    | Sete reg                          -> ["    sete  " ^ reg]
    | Setc reg                          -> ["    setc  " ^ reg]
    | Setz reg                          -> ["    setz  " ^ reg]
    | Setg reg                          -> ["    setg  " ^ reg]
    | Movzx { src ; dst }               -> ["    movzx " ^ dst ^ ", " ^ src]
    | Bin_Arithm { ins ; snd }          -> insn_bin_arithm ins snd
  in String.concat "\n" (List.flatten (List.map insn_str insns))

let execute stmts = 
  String.concat "\n" main_pre ^
  "\n" ^
  insns_str (Optimizer.execute (stmts_insns stmts)) ^
  "\n" ^
  String.concat "\n" main_post