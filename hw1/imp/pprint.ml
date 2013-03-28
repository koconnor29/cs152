open Ast

let rec
  pprintAexp a = print_string(strAexp a)
and
  pprintBexp b = print_string(strBexp b)
and
  pprintCom c = print_string(strCom(0, c))
and
  space n = if n = 0 then "" else " " ^ space(n-1)
and
  strAexp e = match e with
  | Int m -> string_of_int m
  | Var x -> x
  | Plus(a1, a2)  -> ("(" ^   strAexp(a1) ^
                      " + " ^ strAexp(a2) ^ ")")
  | Minus(a1, a2) -> ("(" ^   strAexp(a1) ^
                      " - " ^ strAexp(a2) ^ ")")
  | Times(a1, a2) -> ("(" ^   strAexp(a1) ^
                      " * " ^ strAexp(a2) ^ ")")
and
  strBexp e = match e with
  | True -> "true"
  | False -> "false"
  | Equals(a1, a2) -> ("(" ^   strAexp(a1) ^
                       " = " ^ strAexp(a2) ^ ")")
  | Less(a1, a2)   -> ("(" ^   strAexp(a1) ^
                       " < " ^ strAexp(a2) ^ ")")
  | LessEq(a1, a2) -> ("(" ^   strAexp(a1) ^
                       " <= " ^ strAexp(a2) ^ ")")

and
  strCom(n, c) = match c with
  | Skip -> (space(n) ^ "skip")
  | Assign(x, a) -> ((space n) ^ x ^ " := " ^ strAexp(a))
  | Seq(c1, c2) -> (strCom(n, c1) ^ ";\n" ^ strCom(n, c2))
  | If(b, c1, c2) -> (space(n) ^ "if " ^ strBexp(b) ^ " then {\n" ^
                      strCom(n+2, c1) ^ "\n" ^
                      space(n) ^ "} else {\n" ^
                      strCom(n+2, c2) ^ "\n" ^
                      space(n) ^ "}")
  | While(b, c) -> (space(n) ^ "while " ^ strBexp(b) ^ " do {\n" ^
                    strCom(n+2, c) ^ "\n" ^
                    space(n) ^ "}")
