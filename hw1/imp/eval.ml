open Ast
  
type var = string
exception UnboundVariable of var
    
(* A type for stores (aka variable environments) *)
type store = (var * int) list
      
      
(* Update binding for x in store s. Returns s[x->v] *)
let rec update (s:store) (x:var) (v:int)  =
  match s with
  | [] -> [(x,v)]
  | (y,u)::t ->
    if x = y
    then (x,v)::t
    else (y,u)::(update t x v)
		
(* lookup(s, x) returns the binding for x in store s,       
   and raises UnboundVariable if no such variable exists. *)
let rec lookup (s:store) (x:var) = 
  match s with
  |  [] -> raise (UnboundVariable x)
  |  (y,u)::t -> if x = y then u else lookup t x

(* The evaluation function for arithmetic expressions. Takes expression 
   a, and store s, and returns the resulting int. *)
let rec evala (a:aexp) (s:store) : int = 
  match a with 
    | Int n -> n
    | Var x -> (lookup s x)
    | Plus(a1,a2)  -> (evala a1 s) + (evala a2 s)
    | Minus(a1,a2) -> (evala a1 s) - (evala a2 s)
    | Times(a1,a2) -> (evala a1 s) * (evala a2 s)	

(* The evaluation function for boolean expressions. Takes expression 
   b, and store s, and returns the resulting bool. *)
let rec evalb (b:bexp) (s:store) : bool = 
  match b with
    | True -> true
    | False -> false
    | Equals(a1,a2) -> (evala a1 s) =  (evala a2 s)
    | Less(a1,a2)   -> (evala a1 s) <  (evala a2 s)
    | LessEq(a1,a2) -> (evala a1 s) <= (evala a2 s)

(* The evaluation function for commands. Takes command 
   c, and store s, and returns the resulting store.    *)
let rec evalc c s : store = 
  match c with 
    | Skip -> s
    | Assign(v,e) -> 
      let n = evala e s in
      update s v n
    | Seq(c1,c2) -> 
      let s' = evalc c1 s in
      evalc c2  s'
    | If(b,c1,c2) ->
      if evalb b s  then evalc c1 s 
      else evalc c2 s
    | While(b,c') -> if evalb b s then 
	let s' = evalc c' s in
	evalc c s' 
      else s


    

