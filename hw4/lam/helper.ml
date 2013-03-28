open Ast

(* ---------- EQUALITY ---------- *)

(* [typ_eq t1 t2] returns [true] iff [t1] and [t2] are equal *)  
let rec typ_eq (t1:typ) (t2:typ) : bool = 
  match t1,t2 with 
    | TInt,TInt -> 
      true
    | TBool,TBool -> 
      true
    | TVar x, TVar y -> 
      x = y        
    | TPair(t11,t12),TPair(t21,t22) -> 
      typ_eq t11 t21 && typ_eq t12 t22
    | TArrow(t11,t12),TArrow(t21,t22) -> 
      typ_eq t11 t21 && typ_eq t12 t22
    | _ -> 
      false

(* ---------- SUBSTITUTIONS ----------- *)

(* [subst_typ t0 x t] replaces [TVar x] with [t] in [t0] *)
let rec subst_typ (t0:typ) (x:var) (t:typ) : typ = 
  match t0 with 
    | TInt -> 
      t0
    | TBool -> 
      t0 
    | TVar y -> 
      if x = y then t else t0
    | TPair(t1,t2) -> 
      TPair(subst_typ t1 x t, subst_typ t2 x t)
    | TArrow(t1,t2) -> 
      TArrow(subst_typ t1 x t, subst_typ t2 x t)

(* [apply_subst s t0] applies the substitution [s] to [t0] *)
let rec apply_subst (s:subst) (t0:typ) : typ = 
  match t0 with 
    | TInt -> 
      t0
    | TBool -> 
      t0 
    | TVar y -> 
      (try (apply_subst s (VarMap.find y s))
       with Not_found -> t0)
    | TPair(t1,t2) -> 
      TPair(apply_subst s t1, apply_subst s t2)
    | TArrow(t1,t2) -> 
      TArrow(apply_subst s t1, apply_subst s t2)

(* [subst_constr c0 x t] replaces [TVar x] with [t] in [c0] *)
let rec subst_constr (c0:constr) (x:var) (t:typ) : constr = 
  Constr.fold 
    (fun (t1,t2) -> Constr.add (subst_typ t1 x t, subst_typ t2 x t))
    c0 Constr.empty  

(* ---------- FREE TYPE VARIABLES ---------- *)

(* [ftvs t0] calculates the set of free variables of [t0] *)
let rec ftvs (t0:typ) : varset = 
  match t0 with 
    | TInt -> 
      VarSet.empty
    | TBool -> 
      VarSet.empty
    | TVar y -> 
      VarSet.singleton y
    | TPair(t1,t2) | TArrow(t1,t2) -> 
      VarSet.union (ftvs t1) (ftvs t2)

(* ---------- FRESH TYPE VARIABLES ---------- *)
let tvar_cell = ref 0

(* [next_tvar ()] generates a fresh type variable *)
let next_tvar () : typ = 
  let x = !tvar_cell in 
  let c = Char.chr (x mod 26 + 97) in 
  let n = x / 26 in 
  let s = "'" ^ String.make 1 c in 
  incr tvar_cell;
  let x = 
    if n = 0 then s 
    else s ^ "_" ^ string_of_int n in 
  TVar x
