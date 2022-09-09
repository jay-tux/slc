open Types

(** Checks if a character is a digit, and if it is, returns the digit's value *)
let digit chr = 
  let (int_0, int_9, int_chr) = (Char.code '0', Char.code '9', Char.code chr)
  in if (int_chr >= int_0 && int_chr <= int_9) then Some (int_chr - int_0) else None

(** Checks if a character matches with one of the variable names, and if it is, returns the variable *)
let var = function
  | 'x' -> Some X | 'y' -> Some Y | 'z' -> Some Z | 'a' -> Some A | 'b' -> Some B | 'c' -> Some C
  | _ -> None

(** Lexes an integer consisting of one or more digits; returning the number and the next character to lex *)
let rec lexer_int accu chan = 
  let ch = input_char chan
  in match digit ch with Some v -> lexer_int (10 * accu + v) chan
                       | None   -> (accu, ch)

(** Checks whether a character is a lowercase letter *)
let is_letter ch = 
  let (chi, ai, zi) = (Char.code ch, Char.code 'a', Char.code 'z')
  in chi >= ai && chi <= zi

(** Lexes a word (as many letters as possible; `[a-z]*`); returning the word and the next character to lex *)  
let rec lexer_string accu chan =
  let ch = input_char chan
  in if is_letter ch
     then lexer_string (accu ^ String.make 1 ch) chan
     else (accu, ch)

(** Lexes a keyword; returning the word and the next character to lex; or fails if it's not a keyword *)
let lexer_keyword fst chan =
  if fst |> is_letter |> not then failwith (Printf.sprintf "Unrecognized character '%c" fst)
  else let (kw, next) = lexer_string (String.make 1 fst) chan
       in match kw with "while" -> (WHILE, next)
                      | "do"    -> (DO, next)
                      | "if"    -> (IF, next)
                      | "then"  -> (THEN, next)
                      | "else"  -> (ELSE, next)
                      | "print" -> (PRINT, next)
                      | x       -> failwith (Printf.sprintf "Expected <keyword>, got '%s'" x)
  

(** Runs the lexer on the given input channel and next character; returning a list of tokens *)
let rec lexer chan next = 
  let lexer_help chan = lexer chan (input_char chan)
  in match next with
  | '+' -> PLUS :: lexer_help chan
  | '-' -> MINUS :: lexer_help chan
  | '*' -> TIMES :: lexer_help chan
  | '/' -> DIV :: lexer_help chan
  | '<' -> LT :: lexer_help chan
  | '=' -> EQ :: lexer_help chan
  | '!' -> NOT :: lexer_help chan
  | ':' -> let next = input_char chan in 
           if next = '=' then ASSIGN :: lexer_help chan 
           else failwith (Printf.sprintf "Expected ':=', got ':%c'" next)
  | ';' -> SEMICOLON :: lexer_help chan
  | '(' -> LP :: lexer_help chan
  | ')' -> RP :: lexer_help chan
  | '#' -> []
  | x -> 
      match digit x with 
      | Some v -> let (num, pass) = lexer_int v chan in ATOM (Aint num) :: lexer chan pass
      | None   -> 
          match var x with 
          | Some var -> ATOM (Avar var) :: lexer_help chan
          | None     -> 
              if x = ' ' || x = '\n' || x = '\t' then lexer_help chan
              else
                  let tmp = lexer_keyword x chan
                  in let (key, pass) = tmp
                     in key :: lexer chan pass

(** Wrapper function for the lexer *)
let execute chan = lexer chan (input_char chan) (* Wrapper function for lexer *)