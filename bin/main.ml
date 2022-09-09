let (>>>) f g arg = g (f arg)

(** Wrapper function for the runner *)
let execute (stmts, _) = Runner.execute stmts

let compile (stmts, _) = X86.execute stmts

(** Combines the lexer, parser and executer in one function *)
let run_exec = open_in >>> Lexer.execute >>> Parser.execute >>> execute

let run_comp = open_in >>> Lexer.execute >>> Parser.execute >>> compile

let out = ref ""
let ifile = ref ""
let compile = ref false
let run = ref false
let spec = [
  ("-comp", Arg.Set compile,    "Enables compilation to x86") ;
  ("-exec", Arg.Set run,        "Enables inline running") ;
  ("-out",  Arg.Set_string out, "Sets the output file")
]
let usage = "./project <infile>"

let () = 
  Arg.parse spec (fun x -> ifile := x) usage ;
  if !ifile = "" then failwith "No input given" ;
  if !compile then
    let outf = open_out !out
    in Printf.fprintf outf "%s\n" (run_comp !ifile)
  else
    run_exec !ifile