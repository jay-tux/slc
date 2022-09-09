open Types

(** Converts a token to a human-readable string representation *)
let token_str = function
  | ATOM x -> Printf.sprintf "Atom (%s)" (match x with Avar v -> name_of v | Aint i -> string_of_int i)
  | PLUS -> "Operator (+)" | MINUS -> "Operator (-)" | TIMES -> "Operator (*)" | DIV -> "Operator (/)"
  | LT -> "Operator (<)" | EQ -> "Operator (=)" | NOT -> "Operator (!)" | LP -> "Par. (()" | RP -> "Par. ())"
  | WHILE -> "Keyword (while)" | DO -> "Keyword (do)" | IF -> "Keyword (if)" | THEN -> "Keyword (then)"
  | ELSE -> "Keyword (else)" | PRINT -> "Keyword (print)"
  | ASSIGN -> "Assignment (:=)" | SEMICOLON -> "Terminator (;)"

(** Parses a the first expression in a list of tokens *)
let parse_expr = function
  | ATOM a1 :: PLUS :: ATOM a2 :: rest -> (Ebinop { op = Add ; left = a1 ; right = a2 }, rest)
  | ATOM a1 :: MINUS :: ATOM a2 :: rest -> (Ebinop { op = Sub ; left = a1 ; right = a2 }, rest)
  | ATOM a1 :: TIMES :: ATOM a2 :: rest -> (Ebinop { op = Mul ; left = a1 ; right = a2 }, rest)
  | ATOM a1 :: DIV :: ATOM a2 :: rest -> (Ebinop { op = Div ; left = a1 ; right = a2 }, rest)
  | ATOM a1 :: LT :: ATOM a2 :: rest -> (Ebinop { op = Lt ; left = a1 ; right = a2 }, rest)
  | ATOM a1 :: EQ :: ATOM a2 :: rest -> (Ebinop { op = Equal ; left = a1 ; right = a2 }, rest)
  | NOT :: ATOM a2 :: rest -> (Eunop { op = Not ; arg = a2 }, rest)
  | MINUS :: ATOM a2 :: rest -> (Eunop { op = Neg ; arg = a2 }, rest)
  | ATOM a :: rest -> (Eatom a, rest)
  | _ -> failwith (Printf.sprintf "Expected <expression>.")

(** Parses a list of tokens into a list of statements *)
let rec parse_stmt_list accu tok =
  let parse_stmt = function
  | PRINT :: ATOM (Avar v) :: rest -> (Sprint v, rest)
  | ATOM (Avar v) :: ASSIGN :: rest -> let (e, r) = parse_expr rest in (Sassign { var = v ; value = e }, r)
  | WHILE :: ATOM (Avar v) :: DO :: LP :: rest -> 
      (let (l, r) = parse_stmt_list [] rest 
      in match r with RP :: list  -> (Swhile { condition = v ; body = l }, list)
                    | tok :: _    -> failwith (Printf.sprintf "Expected <right par.>, got '%s'." (token_str tok))
                    | []          -> failwith "Expected <right par.>, got <EOF>")
  | IF :: ATOM (Avar v) :: THEN :: LP :: rest ->
      (let (true_l, r) = parse_stmt_list [] rest
      in match r with RP :: ELSE :: LP :: list -> 
                      (let (false_l, r2) = parse_stmt_list [] list
                      in match r2 with RP :: list2 -> (Sif { condition = v ; true_body = true_l ; false_body = false_l }, list2)
                                     | tok :: _    -> failwith (Printf.sprintf "Expected <right par.>, got '%s'." (token_str tok))
                                     | []          -> failwith "Expected <right par.>, got <EOF>")
                    | RP :: list               -> (Sif { condition = v ; true_body = true_l ; false_body = [] }, list)
                    | tok :: _                 -> failwith (Printf.sprintf "Expected <right par.>, got '%s'" (token_str tok))
                    | []                       -> failwith "Expected <right par.>, got <EOF>")
  | x :: _    -> failwith (Printf.sprintf "Malformed statement starting with '%s'" (token_str x))
  | []        -> failwith "Expected <stmt>, got <EOF>"
  in let (next, rest) = parse_stmt tok
  in match rest with SEMICOLON :: list -> parse_stmt_list (next::accu) list
                   | list              -> (List.rev (next::accu), list)

(** Wrapper function for the parser *)
let execute = parse_stmt_list [] (* Wrapper function for parse_stmt_list *)