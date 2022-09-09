open Types

(** Gets the reference to a variable in the execution context *)
let ref_of =
  let (refx, refy, refz, refa, refb, refc) = (ref 0, ref 0, ref 0, ref 0, ref 0, ref 0) in
  function X -> refx | Y -> refy | Z -> refz | A -> refa | B -> refb | C -> refc

(** Gets the value stored in an atom *)
let atom_val = function
  | Aint a -> a
  | Avar v -> !(ref_of v)

(** Runs a unary operator on a value *)
let run_unop op arg = match op with Neg -> (-(arg))
                                  | Not -> if (arg) = 0 then 1 else 0

(** Runs a binary operator on two arguments *)                                  
let fun_binop op = match op with Add   -> (+)
                               | Sub   -> (-)
                               | Mul   -> fun a b -> a * b
                               | Div   -> (/)
                               | Lt    -> fun a b -> if a < b then 1 else 0
                               | Equal -> fun a b -> if a = b then 1 else 0

(** Executes a single expression and returns it *)
let run_expr = function
  | Eatom a                      -> atom_val a
  | Eunop { op ; arg }           -> run_unop op (atom_val arg)
  | Ebinop { op ; left ; right } -> fun_binop op (atom_val left) (atom_val right)

(** Runs all statements in the given list. *)
let rec run_stmt_list l = 
  let rec run_stmt = function
    | Sprint e                                    -> Printf.printf "%d\n" (run_expr (Eatom (Avar e)))
    | Sassign { var ; value }                     -> (ref_of var) := (run_expr value)
    | Swhile { condition ; body }                 -> if !(ref_of condition) = 1 
                                                     then ((run_stmt_list body) ; (run_stmt (Swhile { condition ; body })))
    | Sif { condition ; true_body ; false_body }  -> if !(ref_of condition) = 1
                                                     then run_stmt_list true_body
                                                     else run_stmt_list false_body
  in List.iter (fun x -> run_stmt x) l

let execute = run_stmt_list