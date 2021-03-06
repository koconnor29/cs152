type var = string

type aexp =
    Int of int
  | Var of var
  | Plus of aexp * aexp
  | Minus of aexp * aexp
  | Times of aexp * aexp

and bexp =
    True
  | False
  | Equals of aexp * aexp
  | Less of aexp * aexp
  | LessEq of aexp * aexp

and com =
    Skip
  | Assign of var * aexp
  | Seq of com * com
  | If of bexp * com * com
  | While of bexp * com

