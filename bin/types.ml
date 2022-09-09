(** Variable type: only 6 possible values *)
type variable = X | Y | Z | A | B | C

(** Atom type: either a variable or an integer literal/value *)
type atom = Avar of variable | Aint of int

(** Unary operator type: only 2 possible values *)
type unop = Neg | Not

(** Binary operator type: only 6 possible values *)
type binop = Add | Sub | Mul | Div | Lt | Equal

(** Expression type*)
type expression = 
| Eatom of atom                                         (** An expression of a single value (an atom) *)
| Eunop of { op : unop ; arg : atom }                   (** An expression which is the application of a unary operator on an argument *)
| Ebinop of { op : binop ; left : atom ; right : atom } (** An expression which is the application of a binary operator on 2 arguments *)

(** Statement type *)
type statement = 
| Sprint of variable (** Print a variable's value *)
| Sassign of { var : variable ; value : expression } (** Assign the result of an expression to a variable *)
| Swhile of { condition : variable ; body : statement list } (** While the variable != 0, execute the body *)
| Sif of { condition : variable ; true_body : statement list ; false_body : statement list } (** If the variable = 1, execute the true_body, otherwise the false_body *)

(** Program type (a list of statements) *)
type program = statement list

(** Token type: either an atom, an operator or one of the keywords *)
type token = 
  ATOM of atom | PLUS | MINUS | TIMES | DIV | LT | EQ | NOT | WHILE | DO | 
  IF | THEN | ELSE | PRINT | LP | RP | ASSIGN | SEMICOLON

(** Gets the name of a variable *)
let name_of = function 
  | X -> "x" | Y -> "y" | Z -> "z" 
  | A -> "a" | B -> "b" | C -> "c"