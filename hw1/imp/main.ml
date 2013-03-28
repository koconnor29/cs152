open Printf;;

if Array.length Sys.argv <> 2 then
  (printf "Usage: imp <file>\n";
   exit 0);;

(* Parse the given file and build the syntax tree (AST)
 * for the command that represents the program.
 * The syntax tree is stored in variable "com". *)
let file = open_in (Sys.argv.(1));;
let lexbuf = Lexing.from_channel file;;
let com = try Parser.program Lexer.token lexbuf
          with Parsing.Parse_error ->
             let pos = lexbuf.Lexing.lex_curr_p in
             printf "Syntax error at line %d\n"
                    pos.Lexing.pos_lnum;
             exit 1;;

(* Pretty-print the command "com" *)
printf "Program:\n\n";;
Pprint.pprintCom com;;
printf "\n\n";;

let storetostring s  = List.fold_left (fun a -> fun (x,v) -> x ^ "=" ^ string_of_int(v) ^ " " ^ a) "" s;;

printf "Evaluating the program...\n";;
flush stdout;;
let final_store = Eval.evalc com [];;
printf "Final store: %s\n" (storetostring final_store);;

