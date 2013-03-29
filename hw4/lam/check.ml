open Ast
open Helper

exception TypeError 
exception UnificationError

(* ---------- UNIFICATION ---------- *)
(* [unify c0] solves [c0] (if possible), yielding a substitution. Raise UnificationError if constraints cannot be unified. *)
let rec unify (c0:constr) : subst = 
    if Constr.is_empty c0 then VarMap.empty else
    let (t1,t2) = Constr.choose c0 in
    let rest = Constr.remove (t1,t2) c0 in
    if typ_eq t1 t2 then unify rest else
        match t1,t2 with
        | TArrow(ta,tb), TArrow(tc,td) -> 
            unify(Constr.add (ta,tc) (Constr.add (tb,td) rest))
        | TPair(ta,tb), TPair(tc,td) ->
            unify(Constr.add (ta,tc) (Constr.add (tb,td) rest))
        | TVar x, _ -> if not (VarSet.mem x (ftvs t2)) then
            VarMap.add x t2 (unify (subst_constr rest x t2))
            else raise UnificationError
        | _, TVar x -> if not (VarSet.mem x (ftvs t1)) then
            VarMap.add x t1 (unify (subst_constr rest x t1))
            else raise UnificationError
        | _ -> raise UnificationError



(* [check g e0] typechecks [e0] in the context [g] generating a type and a set of constraints. Raise TypeError if constraints cannot be generated. *)
let rec check (g:context) (e0:exp) : typ * constr = 
  match e0 with
  | Var x -> if VarMap.mem x g then 
      (VarMap.find x g, Constr.empty) 
      else raise TypeError
  | App(e1,e2) -> 
      let t1, c1 = check g e1 in
      let t2, c2 = check g e2 in
      let x = next_tvar () in
      (x, Constr.add (t1,TArrow(t2,x)) (Constr.union c1 c2))
  | Lam(x,e) -> 
      let x' = next_tvar () in
      let g' = VarMap.add x x' g in
      let t, c =  check g' e in
      (TArrow(x',t), c)
  | Let(x,e1,e2) ->
      let t1, c1 = check g e1 in
      let g' = VarMap.add x t1 g in
      let t2, c2 = check g' e2 in
      (t2, Constr.union c1 c2)
  | Int n -> (TInt, Constr.empty)
  | Plus(e1,e2) | Times(e1,e2) | Minus(e1,e2) -> 
      let t1, c1 = check g e1 in
      let t2, c2 = check g e2 in
      (TInt, Constr.add (t1,TInt) (Constr.add (t2,TInt) (Constr.union c1 c2)))
  | Pair(e1,e2) -> 
      let t1,c1 = check g e1 in
      let t2,c2 = check g e2 in
      (TPair(t1,t2), Constr.union c1 c2)
  | Fst e -> 
      let t, c = check g e in
      (match t with
      | TPair(t1,t2) -> (t1,c)
      | TVar x -> 
          let x1 = next_tvar () in
          let x2 = next_tvar () in
          (x1, Constr.add (TVar x,TPair(x1,x2)) c)
      | _ -> raise TypeError)
  | Snd e ->
      let t, c = check g e in
      (match t with
      | TPair(t1,t2) -> (t2,c)
      | TVar x -> 
          let x1 = next_tvar () in
          let x2 = next_tvar () in
          (x2, Constr.add (TVar x,TPair(x1,x2)) c)
      | _ -> raise TypeError )
  | True | False -> (TBool, Constr.empty)
  | Eq(e1,e2) -> 
      let t1, c1 = check g e1 in
      let t2, c2 = check g e2 in
      (TBool, Constr.add (t1,TInt) (Constr.add (t2,TInt) (Constr.union c1 c2)))
  | If(e1,e2,e3) -> 
      let t1, c1 = check g e1 in
      let t2, c2 = check g e2 in
      let t3, c3 = check g e3 in
      (t2, Constr.add (t1,TBool) (Constr.add (t2,t3) (Constr.union c1
      (Constr.union c2 c3))))
  | Letrec(f,x,e1,e2) ->
      let x1 = next_tvar () in
      let x2 = next_tvar () in
      let g' = VarMap.add f (TArrow(x1,x2)) g in
      let t1, c1 = check g' e1 in
      let t2, c2 = check g' e2 in
      (t2, Constr.add (x2, t1) (Constr.union c1 c2))
  | _ -> raise TypeError
