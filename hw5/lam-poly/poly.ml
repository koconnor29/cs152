open Ast
open Helper

exception TypeError
exception UnificationError

(* ---------- UNIFICATION ---------- *)
(* [unify c0] solves [c0] (if possible), yielding a
   substitution. Raise UnificationError if constraints cannot be
   unified. *)
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



(* [quantify g t] takes a context [g] and a type [t] and finds all
   type variables in [t] that are also not used in [g] and forms a type
   scheme from these type variables and [t] *)
let quantify (g:context) (t:typ) : typ_scheme =
  let frees_t = ftvs t in
  let frees_g = ftvs_context g in
  (VarSet.diff frees_t frees_g, t)

(* [instantiate s] takes a type scheme generates fresh type variables
   for any quantified type variables in [s] and returns a new type with
   those fresh variables substituted for the quantified ones *)
let rec instantiate (ts:typ_scheme) : typ =
  let vset, t = ts in
  if VarSet.is_empty vset then t else
    let x = VarSet.choose vset in
    let vset' = VarSet.remove x vset in
    let t' = subst_typ t x (next_tvar ()) in
    instantiate (vset',t')

(* [check g e0] typechecks [e0] in the context [g] generating a type
   and a set of constraints. Raise TypeError if constraints cannot be
   generated. *)
let rec check (g:context) (e0:exp) : typ * constr =
  match e0 with
  | Var x -> if VarMap.mem x g then 
    (instantiate (VarMap.find x g), Constr.empty)  
    else raise TypeError
  | App(e1,e2) -> 
      let t1, c1 = check g e1 in
      let t2, c2 = check g e2 in
      let x = next_tvar () in
      (x, Constr.add (t1,TArrow(t2,x)) (Constr.union c1 c2))
  | Lam(x,e) -> 
      let x' = next_tvar () in
      let g' = VarMap.add x (VarSet.empty, x') g in
      let t, c =  check g' e in
      (TArrow(x',t), c)
  | Let(x,e1,e2) ->
      let t1,c1 = check g e1 in
      let s = unify c1 in
      let t1' = apply_subst s t1 in
      let ts = quantify g t1' in
      let g' = VarMap.add x ts g in
      let t2,c2 = check g' e2 in 
      (t2,Constr.union c1 c2)
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
      let x1 = next_tvar () in
      let x2 = next_tvar () in
      (x1,Constr.add (t,TPair(x1,x2)) c)
  | Snd e ->
      let t, c = check g e in
      let x1 = next_tvar () in
      let x2 = next_tvar () in
      (x2,Constr.add (t,TPair(x1,x2)) c)
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
